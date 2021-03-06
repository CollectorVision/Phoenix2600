-------------------------------------------------------------------[13.08.2016]
-- VGA
-------------------------------------------------------------------------------
-- Engineer: MVV <mvvproject@gmail.com>
--
-- Erik Piehl, Feb 2019
-- Modified for collectorvision for timing generator for the VGA input for HDMI...
-- Now uses 32K frambuffer. 
-- EP 2019-03-24 NOTE: THIS FRAMEBUFFER IS NOW 38K, NOT 32K to support 240 scanlines.
-- EP 2019-04-20 NOTE: THIS FRAMEBUFFER IS NOW 42K, NOT 38K to support 268 scanlines.

library IEEE; 
	use IEEE.std_logic_1164.all; 
	use IEEE.std_logic_unsigned.all;
	use IEEE.numeric_std.all;
	
entity vga is
	generic (
		-- Within I_HCNTthere is offset. We substract the offset
		-- and write to memory when outside the offsets.
		h_input_offset		: integer := 48 -- 68;
	);
	port (
		I_CLK_VGA	: in  std_logic;
		I_CLK_VGA2X : in  std_logic;
		I_PX_CLK    : in  std_logic;
		I_COLOR		: in  std_logic_vector(6 downto 0);
		I_HCNT		: in  unsigned(7 downto 0);
		I_VCNT		: in  unsigned(8 downto 0);	-- Aciddrop has 272 scanlines.
		I_VSYNC		: in  std_logic;
		I_LAST_VCNT : in  unsigned(8 downto 0);	-- Last measure height of the screen
		O_HSYNC		: out std_logic;
		O_VSYNC		: out std_logic;
		O_COLOR		: out std_logic_vector(6 downto 0);
		O_BLANK		: out std_logic
	);
end vga;

architecture rtl of vga is
	signal pixel_out		: std_logic_vector( 6 downto 0);
	signal addr_rd			: std_logic_vector(15 downto 0);	-- upgraded to 38K framebuffer, 16-bit address
	signal addr_wr			: std_logic_vector(15 downto 0); -- upgraded to 38K framebuffer, 16-bit address
	signal wren				: std_logic;
	signal picture			: std_logic;
	signal window_hcnt	: std_logic_vector( 9 downto 0) := (others => '0');
	signal window_vcnt	: std_logic_vector( 9 downto 0) := (others => '0');
	signal hcnt				: std_logic_vector( 9 downto 0) := (others => '0');
	signal h					: std_logic_vector( 9 downto 0) := (others => '0');
	signal vcnt				: std_logic_vector( 9 downto 0) := (others => '0');
	signal hsync			: std_logic;
	signal vsync			: std_logic;
	signal blank			: std_logic;
	
	signal toggler 		: std_logic := '0';
	signal buf_pixel_outx2: std_logic_vector( 6 downto 0);
	signal buf_pixel_out : std_logic_vector( 6 downto 0);
	signal pixel_sample_timer : std_logic_vector(7 downto 0) := x"00";
	
	signal downscale_y 	: boolean := false;	-- set to true if  image needs to be shrunken

-- ModeLine "640x480@60Hz"  25,175  640  656  752  800 480 490 492 525 -HSync -VSync
	-- Horizontal Timing constants  
--	constant h_pixels_across	: integer := 640 - 1;
--	constant h_sync_on			: integer := 656 - 1;
--	constant h_sync_off			: integer := 752 - 1;
--	constant h_end_count			: integer := 800 - 1;
--	-- Vertical Timing constants
--	constant v_pixels_down		: integer := 480 - 1;
--	constant v_sync_on			: integer := 490 - 1;
--	constant v_sync_off			: integer := 492 - 1;
--	constant v_end_count			: integer := 525 - 1;

-- Creating 720x480@60Hz mode
	constant h_pixels_across	: integer := 720      - 1;
	constant h_sync_on			: integer := 720 + 16 - 1;
	constant h_sync_off			: integer := 720 + 16 + 62 - 1;
	constant h_end_count			: integer := 858 - 1;
--	-- Vertical Timing constants
	constant v_pixels_down		: integer := 480 - 1;
	constant v_sync_on			: integer := 489 - 1;
	constant v_sync_off			: integer := 495 - 1;
	constant v_end_count			: integer := 525 - 1;

-----------------------------------------------------

	-- In
	constant hc_max				: integer := 160;
	constant vc_max				: integer := 268;

	constant h_start				: integer := 40;		-- 2 when 640x480
	constant h_end					: integer := h_start + (hc_max * 4);	
	constant v_start				: integer := 0; -- 22;
	constant v_end					: integer := v_start + (vc_max * 2);
	
	COMPONENT dualport8k
	  PORT (
		 clka : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 clkb : IN STD_LOGIC;
		 web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addrb : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dinb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;
	
	COMPONENT simple_dualport_32k -- extended to 38k with 16-bit address bus
	  PORT (
		 clka : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 clkb : IN STD_LOGIC;
		 addrb : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;	
	
	COMPONENT dualport32k 
	PORT (
		clka : IN STD_LOGIC;
		wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		addra : IN STD_LOGIC_VECTOR(14 DOWNTO 0);
		dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		clkb : IN STD_LOGIC;
		web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		addrb : IN STD_LOGIC_VECTOR(14 DOWNTO 0);
		dinb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);
	END COMPONENT;
		
begin

	frbuff:  simple_dualport_32k port map (
    clka 	=> I_CLK_VGA, -- I_CLK_VGA2X,
    wea(0)	=> wren, 
    addra 	=> addr_wr,
    dina 	=> '0' & I_COLOR,
    clkb 	=> I_CLK_VGA,
    addrb 	=> addr_rd,
	 doutb(7) => open,
    doutb(6 downto 0) => pixel_out
	);
	

	process (I_CLK_VGA)
	begin
		if rising_edge(I_CLK_VGA) then
			pixel_sample_timer <= pixel_sample_timer(6 downto 0) & I_PX_CLK;
			buf_pixel_out <= pixel_out; -- buf_pixel_outx2;	-- pixels change at the edge of the 25MHz clock
			
			if hcnt = h_end_count then
				hcnt <= (others => '0');
			else
				hcnt <= hcnt + 1;
				if h_start /= 0 and hcnt = (h_start-1) then
					-- reset window counter here if h_start is not zero.
					window_hcnt <= (others => '0');
				else
					window_hcnt <= window_hcnt + 1;
				end if;
			end if;
			if hcnt = h_sync_on then
				if vcnt = v_end_count then
					vcnt <= (others => '0');
					if v_start = 0 then
						window_vcnt <= (others => '0');
					end if;
				else
					vcnt <= vcnt + 1;
					if vcnt = (v_start-1) then
						window_vcnt <= (others => '0');
					else 
						-- test code - advance window_vcnt a little faster - BUGBUG
						if downscale_y and vcnt(1 downto 0) = "11" then
							window_vcnt <= window_vcnt + 2;	
						else
							window_vcnt <= window_vcnt + 1;
						end if;
					end if;
				end if;
			end if;
		end if; -- if rising_edge
	end process;


	process (I_HCNT, I_VCNT, window_hcnt, window_vcnt)
		variable wr_result_v : std_logic_vector(17 downto 0);
		variable rd_result_v : std_logic_vector(17 downto 0);
	begin
		wr_result_v := std_logic_vector(I_VCNT * 160 + (I_HCNT - h_input_offset));
		rd_result_v := std_logic_vector((unsigned(window_vcnt(9 downto 1)) * 160) + unsigned(window_hcnt(9 downto 2)));
		addr_wr	<= wr_result_v(15 downto 0);
		addr_rd	<= rd_result_v(15 downto 0);	
	end process;

--	wren		<= '1' when pixel_sample_timer(3 downto 0) = "0011" 	-- we've seen the rising edge of pixel clock (3.6MHz) 
	wren		<= '1' when pixel_sample_timer(1 downto 0) = "0011" 	-- we've seen the rising edge of pixel clock (3.6MHz) 
							   and (I_HCNT >= h_input_offset) and (I_HCNT < hc_max+h_input_offset) 
						      and (I_VCNT < vc_max)
						 else '0';	
	blank		<= '1' when (hcnt > h_pixels_across) or (vcnt > v_pixels_down) else '0';
	picture	<= '1' when (blank = '0') and (hcnt > h_start+1 and hcnt < h_end) 
	                                   and (vcnt >= v_start and vcnt < v_end) 
							-- The condition below blanks the pixels after reaching the last scanline. Special cased for Acid drop etc which can have more than 240 scanlines
							and ((not downscale_y and (unsigned(window_vcnt(9 downto 1)) < I_LAST_VCNT-60)) or (downscale_y and window_vcnt(9 downto 1) < 268))
						   else '0';

	O_HSYNC	<= '1' when (hcnt <= h_sync_on) or (hcnt > h_sync_off) else '0';
	O_VSYNC	<= '1' when (vcnt <= v_sync_on) or (vcnt > v_sync_off) else '0';
	O_COLOR  <= -- DEBUG: buf_pixel_out xor "1111111" when (window_vcnt=428 or window_vcnt=427) and downscale_y else
			      buf_pixel_out 					 when picture = '1' else 
					(others => '0');
	O_BLANK	<= blank;
	
	downscale_y <= I_LAST_VCNT-70 > 244 ;

end rtl;
