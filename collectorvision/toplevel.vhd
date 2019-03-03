--
-- toplevel.vhd
-- 	formerly ZXUNO_A2601.vhd
--
-- Atari VCS 2600 toplevel for the Collectorvision
--
-- Modified from ZXUNO version 2016 DistWave
-- Extensively modifed for the Collectorvision Phoenix by Erik Piehl 2018-2019
--		support for external SRAM
--
-- Based on the MiST port from https://github.com/wsoltys/tca2601
-- Copyright (c) 2014 W. Soltys <wsoltys@gmail.com>
--
-- This source file is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

library unisim;
use unisim.vcomponents.all;

-- -----------------------------------------------------------------------

entity toplevel is
    port (
    
-- Clock
      CLOCK_50_i : in std_logic;
		
-- Buttons
		btn_reset_n_i		: in    std_logic;

-- SPI
		sd_cs_n_o : out std_logic;
		sd_sclk_o : out std_logic;
		sd_mosi_o : out std_logic;
		sd_miso_i : in std_logic := '1';
		sd_cd_n_i : in std_logic := '1';

-- LED
      led_o : out std_logic;
      led2_o : out std_logic;

-- Video
--      VGA_R : out std_logic_vector(2 downto 0);
--      VGA_G : out std_logic_vector(2 downto 0);
--      VGA_B : out std_logic_vector(2 downto 0);
--      VGA_HS : out std_logic;
--      VGA_VS : out std_logic;
--		NTSC   : out   std_logic; 
--      PAL    : out   std_logic;

-- Joystick
--      P_L: in std_logic;
--      P_R: in std_logic;
--      P_A: in std_logic;
--      P_U: in std_logic;
--      P_D: in std_logic;
--		P_tr: inout std_logic;
		
		-- Joystick
		joy_p5_o				: out   std_logic;
		joy_p8_o				: out   std_logic;
		joy1_p1_i			: in    std_logic;
		joy1_p2_i			: in    std_logic;
		joy1_p3_i			: in    std_logic;
		joy1_p4_i			: in    std_logic;
		joy1_p6_i			: in    std_logic;
		joy1_p7_i			: in    std_logic;
		joy1_p9_i			: in    std_logic;
		joy2_p1_i			: out    std_logic;
		joy2_p2_i			: in    std_logic;
		joy2_p3_i			: in    std_logic;
		joy2_p4_i			: out    std_logic;
		joy2_p6_i			: in    std_logic;
		joy2_p7_i			: in    std_logic;
		joy2_p9_i			: in    std_logic;
		

-- Audio
      dac_l_o : out std_logic;
      dac_r_o : out std_logic;
		
-- PS2
      ps2_clk_io : in std_logic;
      ps2_data_io : in std_logic;

-- Serial Flash
		flash_cs_n_o : out std_logic;
		flash_sclk_o : out std_logic;
		flash_mosi_o : out std_logic;
		flash_miso_i : in std_logic;
		flash_wp_o   : out std_logic;
		flash_hold_o : out std_logic;
		
		-- VGA
		vga_r_o				: out   std_logic_vector(3 downto 0)	:= (others => '0');
		vga_g_o				: out   std_logic_vector(3 downto 0)	:= (others => '0');
		vga_b_o				: out   std_logic_vector(3 downto 0)	:= (others => '0');
		vga_hsync_n_o		: out   std_logic								:= '1';
		vga_vsync_n_o		: out   std_logic								:= '1';
		stnd_o				: out   std_logic								:= '1';
		pal_clk_en_o		: out   std_logic								:= '1';

		-- HDMI
		hdmi_p_o				: out   std_logic_vector(3 downto 0);
		hdmi_n_o				: out   std_logic_vector(3 downto 0);

		-- Cartridge
		cart_addr_o			: out   std_logic_vector(14 downto 0)	:= (others => '0');
		cart_data_i			: in    std_logic_vector( 7 downto 0);
		cart_dir_o			: out   std_logic								:= '1';
		cart_oe_n_o			: out   std_logic								:= '1';
		cart_en_80_n_o		: out   std_logic								:= '1';
		cart_en_A0_n_o		: out   std_logic								:= '1';
		cart_en_C0_n_o		: out   std_logic								:= '1';
		cart_en_E0_n_o		: out   std_logic								:= '1';
		
		
-- SRAM
		sram_addr_o	: out std_logic_vector(20 downto 0);
		sram_data_io : inout std_logic_vector(7 downto 0) := "ZZZZZZZZ";
		sram_oe_n_o : out std_logic := '1';
		sram_we_n_o : out std_logic := '1'
		
    );
end entity;

-- -----------------------------------------------------------------------

architecture rtl of toplevel is

-- System clocks
  signal clk_50_buffered: std_logic;
  signal vid_clk        : std_logic := '0';
  signal vid_clk_25M    : std_logic;
  signal vid_clk_125M_p : std_logic;
  signal vid_clk_125M_n : std_logic;

-- A2601
  signal audio: std_logic := '0';

  signal p_b: std_logic := '0';
  signal p2_l: std_logic := '0';
  signal p2_r: std_logic := '0';
  signal p2_a: std_logic := '0';
  signal p2_b: std_logic := '0';
  signal p2_u: std_logic := '0';
  signal p2_d: std_logic := '0';
  
  signal p_color: std_logic := '1';
  signal p_pal: std_logic := '0';
  signal p_dif: std_logic_vector(1 downto 0) := (others => '0');
  signal size: std_logic_vector(15 downto 0) := (others => '0');

-- User IO
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
--  signal joy0       : std_logic_vector(7 downto 0);
--  signal joy1       : std_logic_vector(7 downto 0);
  signal joy_a_0    : std_logic_vector(15 downto 0);
  signal joy_a_1    : std_logic_vector(15 downto 0);
--  signal status     : std_logic_vector(7 downto 0);
  signal ascii_new  : std_logic;
  signal ascii_code : STD_LOGIC_VECTOR(6 DOWNTO 0);
  signal clk12k     : std_logic;
--  signal ps2Clk     : std_logic;
--  signal ps2Data    : std_logic;
--  signal ps2_scancode : std_logic_vector(7 downto 0);
  
  signal ps2k_clk_in : std_logic;
  signal ps2k_dat_in : std_logic;

--CtrlModule--
-- Internal video signals:  
  signal vga_vsync_i : std_logic := '0';
  signal vga_hsync_i : std_logic := '0';
  signal vga_red_i : std_logic_vector(7 downto 0) := (others => '0');
  signal vga_green_i : std_logic_vector(7 downto 0) := (others => '0');
  signal vga_blue_i	: std_logic_vector(7 downto 0) := (others => '0');
  
  -- internal video signals between VGA and HDMI
  signal red8   : std_logic_vector(7 downto 0);
  signal green8 : std_logic_vector(7 downto 0);
  signal blue8  : std_logic_vector(7 downto 0);
  signal hdmi_vga_blank_s : std_logic;
  -- yet more internal signals used for VGA timing generator for HDMI
  signal hdmi_vga_hsync_n_s : std_logic;
  signal hdmi_vga_vsync_n_s : std_logic;
  signal hdmi_vga_lum_hue_color_out : std_logic_vector(6 downto 0);	-- 7 bit color from A2600 core
  -- sync and color signals before entering the VGA scandoubler inside the TIA block
  signal pre_hsyn : std_logic;
  signal	pre_vsyn : std_logic;
  signal	pre_colu : std_logic_vector(6 downto 0);
  
  signal osd_window : std_logic;
  signal osd_pixel : std_logic;
 
  signal scanlines : std_logic;

-- Host control signals, from the Control module
  signal host_reset_n: std_logic;
  signal host_divert_sdcard : std_logic;
  signal host_divert_keyboard : std_logic;
  signal host_pal : std_logic;
  signal host_select : std_logic;
  signal host_start : std_logic;

  signal host_bootdata : std_logic_vector(31 downto 0);
  signal host_bootdata_req : std_logic;
  signal host_bootdata_ack : std_logic;
  
  signal host_bootread_data 	: std_logic_vector(31 downto 0);
  signal host_bootread_addr 	: std_logic_vector(20 downto 0);
  signal host_bootread_req 	: std_logic;
  signal host_bootread_ack 	: std_logic := '0';  
  
-- EP support for Colecovision controller
	signal P_L: std_logic := '1';
	signal P_R: std_logic := '1';
	signal P_A: std_logic := '1';
	signal P_U: std_logic := '1';
	signal P_D: std_logic := '1';
	-- EP HDMI signals
	signal clock_vga_s		: std_logic;
	signal clock_hdmi_s		: std_logic;
	signal clock_hdmi_n_s	: std_logic;

	signal tdms_r_s			: std_logic_vector( 9 downto 0) := (others => '0');
	signal tdms_g_s			: std_logic_vector( 9 downto 0) := (others => '0');
	signal tdms_b_s			: std_logic_vector( 9 downto 0) := (others => '0');
	
	signal sound_hdmi_s		: std_logic_vector(15 downto 0) := (others => '0');
	
	signal a2600_addr			: std_logic_vector(14 downto 0);
	signal a2600_romdata		: std_logic_vector(7 downto 0);
	signal pacman_rom_byte  : std_logic_vector(7 downto 0);
	signal extram_rom_byte  : std_logic_vector(7 downto 0);
	signal rom_loaded			: std_logic;
	
	signal host_mux : std_logic;
	signal host_debug_arm : std_logic;
	signal top_ph0 : std_logic;
	
	type debug_type is array(natural range 0 to 15) of std_logic_vector(31 downto 0);
	signal debug_mem : debug_type;
	signal capt_i : integer range 0 to 15 := 0;
	signal last_6502_addr : std_logic_vector(14 downto 0);
	signal armed : boolean := true;
	signal last_ph0 : std_logic;
	
	signal last_hsync : std_logic := '0';
	signal last_vsync : std_logic := '0';
	signal tia_hcnt : unsigned(7 downto 0);
	signal tia_vcnt : unsigned(7 downto 0);
	signal tia_divider : unsigned(3 downto 0) := "0000";
	signal rgb_color : std_logic_vector(23 downto 0);
	
	signal numpad_0 : std_logic_vector(11 downto 0) := (others => '1');
	signal scan_state : unsigned(7 downto 0) := x"00";
	
	component VGAColorTable is
	port (
		clk : in std_logic; -- Clock input	
		lum : in std_logic_vector(3 downto 0); -- luminance
		hue : in std_logic_vector(3 downto 0); -- hue
		mode : in std_logic_vector(1 downto 0); -- Mode (0 = NTSC, 1 = PAL, 2 = SECAM)
		outColor : out std_logic_vector(23 downto 0) -- 24 bit color output
		);
	end component;
	
	
begin

-- input buffer for 50MHz clock
   IBUFG_inst : IBUFG
   generic map (
      IBUF_LOW_PWR => TRUE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD => "DEFAULT")
   port map (
      O => clk_50_buffered, -- Clock buffer output
      I => CLOCK_50_i  -- Clock buffer input (connect directly to top-level port)
   );

-- logic
	ps2k_dat_in <= ps2_data_io;
	ps2k_clk_in <= ps2_clk_io;
	
	joy2_p1_i <= pre_hsyn; -- pre_hsyn; -- ps2_clk_io;
	joy2_p4_i <= tia_hcnt(0); -- pre_vsyn; -- ps2_data_io;
	
-- Serial flash - not used right now
	flash_cs_n_o <= '1';
	flash_sclk_o <= '0';
	flash_mosi_o <= '0';
	flash_wp_o   <= '0';
	flash_hold_o <= '0';
		
	-- HDMI clocks
	clock_vga_s 	<= vid_clk_25M;
	clock_hdmi_s 	<= vid_clk_125M_p;
	clock_hdmi_n_s <= vid_clk_125M_n;
	
	hdmio: entity work.hdmi_out_xilinx
	port map (
		clock_pixel_i		=> clock_vga_s,
		clock_tdms_i		=> clock_hdmi_s,
		clock_tdms_n_i		=> clock_hdmi_n_s,
		red_i					=> tdms_r_s,
		green_i				=> tdms_g_s,
		blue_i				=> tdms_b_s,
		tmds_out_p			=> hdmi_p_o,
		tmds_out_n			=> hdmi_n_o
	);
	
-- Colecovision gamepad support
	process(vid_clk_25M)
	variable sel : std_logic_vector(3 downto 0);
	begin
		if rising_edge(vid_clk_25M) then
			scan_state <= scan_state + 1;
			if scan_state(6 downto 0) = "0000000" then
				joy_p8_o <= scan_state(7);
				joy_p5_o <= not scan_state(7);
			else
				if scan_state(7) = '0' then
					-- read directions and fire
					P_L	<= joy1_p3_i;
					P_R	<= joy1_p4_i;
					P_A 	<= joy1_p6_i;	-- left fire
					P_U 	<= joy1_p1_i;
					P_D 	<= joy1_p2_i;
				else
					-- read number pad
					numpad_0 <= (others => '1'); -- assume all not pressed
					sel := joy1_p4_i & joy1_p3_i & joy1_p2_i & joy1_p1_i; 
					case sel is
						when "1110" => numpad_0(0) <= '0';	-- key 1
						when "1101" => numpad_0(1) <= '0';	-- key 2
						when "0110" => numpad_0(2) <= '0';  -- key 3
						when "0001" => numpad_0(3) <= '0';	-- key 4
						when "1001" => numpad_0(4) <= '0';	-- key 5
						when "0111" => numpad_0(5) <= '0';	-- key 6
						when "1100" => numpad_0(6) <= '0';	-- key 7
						when "1000" => numpad_0(7) <= '0';	-- key 8
						when "1011" => numpad_0(8) <= '0';	-- key 9
						when "1010" => numpad_0(9) <= '0';	-- key *
						when "0011" => numpad_0(10)<= '0';	-- key 0
						when "0101" => numpad_0(11)<= '0';	-- key #
						when others =>
					end case;
				end if;
			end if;
		end if;
	end process;
	
-- Control module

MyCtrlModule : entity work.CtrlModule
	port map (
		clk => vid_clk,
		reset_n => '1',

		-- Video signals for OSD
		vga_hsync => hdmi_vga_hsync_n_s, -- vga_hsync_i,
		vga_vsync => hdmi_vga_vsync_n_s, -- vga_vsync_i,
		osd_window => osd_window,
		osd_pixel => osd_pixel,

		-- PS2 keyboard
		ps2k_clk_in => ps2k_clk_in,
		ps2k_dat_in => ps2k_dat_in,
		
		-- SD card signals
		spi_clk  => sd_sclk_o,
		spi_mosi => sd_mosi_o,
		spi_miso => sd_miso_i,
		spi_cs   => sd_cs_n_o,
		
		-- DIP switches
		dipswitches(15 downto 5) => open,
		dipswitches(4) => p_dif(1),
		dipswitches(3) => p_dif(0),
		dipswitches(2) => scanlines,
		dipswitches(1) => p_pal,
		dipswitches(0) => p_color,
		
		--ROM size
		size => size,
		
		-- Control signals
		host_divert_keyboard => host_divert_keyboard,
		host_divert_sdcard => host_divert_sdcard,
		host_reset_n => host_reset_n,
		host_start => host_start,
      host_select => host_select,
		
		host_mux => host_mux,
		host_debug_arm => host_debug_arm,
		
		-- Boot data read signals (verification)
		host_bootread_data =>  host_bootread_data,
		host_bootread_addr =>  host_bootread_addr,
		host_bootread_req  =>  host_bootread_req,
		host_bootread_ack  =>  host_bootread_ack,
		
		-- Boot data upload signals
		host_bootdata => host_bootdata,
		host_bootdata_req => host_bootdata_req,
		host_bootdata_ack => host_bootdata_ack
	);

	-- Drive the VGA outputs
	-- this is the original code, timing determined by the A2600 core.
	vga_r_o <= red8(7 downto 4);
	vga_g_o <= green8(7 downto 4);
	vga_b_o <= blue8(7 downto 4);
-- vga_hsync_n_o <= vga_hsync_i;
-- vga_vsync_n_o <= vga_vsync_i;

	-- source secondary timing generator driving HDMI
--	vga_r_o <= rgb_color(23 downto 20);
--	vga_g_o <= rgb_color(15 downto 12);
--	vga_b_o <= rgb_color(7 downto 4);
   vga_hsync_n_o <= hdmi_vga_hsync_n_s;
   vga_vsync_n_o <= hdmi_vga_vsync_n_s;
	
overlay : entity work.OSD_Overlay
	port map
	(
		clk 				=> clock_vga_s, 
		red_in 			=> rgb_color(23 downto 16), 
		green_in 		=> rgb_color(15 downto  8),
		blue_in 			=> rgb_color( 7 downto  0),
		window_in 		=> '1',
		osd_window_in 	=> osd_window,
		osd_pixel_in 	=> osd_pixel,
		hsync_in 		=> hdmi_vga_hsync_n_s,
		red_out 			=> red8,
		green_out 		=> green8,
		blue_out 		=> blue8,
		window_out 		=> open,
		scanline_ena 	=> scanlines
	);	

--overlay : entity work.OSD_Overlay
--	port map
--	(
--		clk 				=> vid_clk,
--		red_in 			=> vga_red_i,
--		green_in 		=> vga_green_i,
--		blue_in 			=> vga_blue_i,
--		window_in 		=> '1',
--		osd_window_in 	=> osd_window,
--		osd_pixel_in 	=> osd_pixel,
--		hsync_in 		=> vga_hsync_i,
--		red_out 			=> red8,
--		green_out 		=> green8,
--		blue_out 		=> blue8,
--		window_out 		=> open,
--		scanline_ena 	=> scanlines
--	);

-- -----------------------------------------------------------------------
-- PACMAN ROM used during bootup
-- -----------------------------------------------------------------------	
	pacmanROM : entity work.pacman_rom
		port map (
			clka => vid_clk,
			addra => a2600_addr(11 downto 0),
			douta => pacman_rom_byte
		);

	-- a2600_romdata <= pacman_rom_byte; -- BUGBUG 
	-- EP 2019-02-16 running again with pacman_rom to test things out.
   a2600_romdata <= extram_rom_byte when host_mux = '1' else pacman_rom_byte;

-- -----------------------------------------------------------------------
-- External SRAM controller
-- -----------------------------------------------------------------------	
	extSRAM : entity work.sram_controller 
		port map (
			clk_i => vid_clk,
			reset_i => not(host_reset_n),

			sram_addr_o	 => sram_addr_o,
			sram_data_io => sram_data_io,
			sram_oe_n_o  => sram_oe_n_o,
			sram_we_n_o  => sram_we_n_o,
			
			host_bootdata => host_bootdata,
			host_bootdata_req => host_bootdata_req,
			host_bootdata_ack => host_bootdata_ack,
			
			host_bootread_data =>  open, -- bugbug host_bootread_data,
			host_bootread_addr =>  host_bootread_addr,
			host_bootread_req  =>  host_bootread_req,
			host_bootread_ack  =>  host_bootread_ack,			
			
			rom_loaded_o => rom_loaded, 
			
			a2600_addr_i => '0' & a2600_addr,
			a2600_data_o => extram_rom_byte
		);
	
-- -----------------------------------------------------------------------
-- A2601 core
-- -----------------------------------------------------------------------
  a2601Instance : entity work.A2601NoFlash
    port map (
      vid_clk => vid_clk,
      audio => audio,
      O_VSYNC => vga_vsync_i,
      O_HSYNC => vga_hsync_i,
      O_VIDEO_R => vga_red_i(7 downto 2),
      O_VIDEO_G => vga_green_i(7 downto 2),
      O_VIDEO_B => vga_blue_i(7 downto 2),
      res => not(host_reset_n),
      p_l => P_L,
      p_r => P_R,
      p_a => P_A,
      p_b => '1',
      p_u => P_U,
      p_d => P_D,
      p2_l => p2_l,
      p2_r => p2_r,
      p2_a => p2_a,
      p2_b => p2_b,
      p2_u => p2_u,
      p2_d => p2_d,
      paddle_0 => joy_a_0(15 downto 8),
      paddle_1 => joy_a_0(7 downto 0),
      paddle_2 => joy_a_1(15 downto 8),
      paddle_3 => joy_a_1(7 downto 0),
      paddle_ena => '0',
      p_start => not(host_start),
      p_select => not(host_select),
      p_color => p_color,
      pal => p_pal,
      p_dif => p_dif,
		a2600_cpu_addr_o => a2600_addr,
		a2600_cpu_data_i => a2600_romdata,
--      bootdata => host_bootdata,
--      bootdata_req => host_bootdata_req,
--      bootdata_ack => host_bootdata_ack,
		show_ph0 => top_ph0,
		-- EP added outputs before VGA scandoubler
		pre_hsyn => pre_hsyn,
		pre_vsyn => pre_vsyn,
		pre_colu => pre_colu,
		-- EP end addition
		size => size
    );

  dac_l_o <= audio;
  dac_r_o <= audio;
  led_o 	<= '0';
  led2_o <= '0';

-- TO-DO: Player 2 controls
  p2_l <= '1';
  p2_r <= '1';
  p2_a <= '1';
  p2_b <= '1';
  p2_u <= '1';
  p2_d <= '1';
  -- P_tr <= 'Z'; -- EP

-- -----------------------------------------------------------------------
-- Clocks and PLL
-- -----------------------------------------------------------------------
  pll_575_instance : entity work.pll
    port map (
      CLK_IN1 => clk_50_buffered,
      CLK_OUT1 => vid_clk			-- 57.5 MHz
		); 
			
	pll_hdmi_instance: entity work.pll_hdmi
	  port map (
	   CLK_IN1  => clk_50_buffered,
		CLK_25   => vid_clk_25M,	 -- 25 MHz
		CLK_125P => vid_clk_125M_p, -- 125 MHz
		CLK_125M => vid_clk_125M_n  -- 125 MHz with 180 degree phase shift
    );


---- debug stuff ----
	process(vid_clk)
	variable t : std_logic_vector(31 downto 0);
	begin
		if host_reset_n = '0' then
			last_6502_addr <= (others => '0');
			capt_i <= 0;
			armed  <= false;
			if host_debug_arm = '1' then	-- if during reset host_debug_arm is set we arm.
				armed <= true;
			end if;
			last_ph0 <= '0';
		elsif rising_edge(vid_clk) then
			last_ph0 <= top_ph0;
			if armed and last_ph0='0' and top_ph0='1' then
				debug_mem(capt_i) <= a2600_romdata & x"00" & '0' & a2600_addr;
				if last_6502_addr /= a2600_addr then
					last_6502_addr <= a2600_addr;
					capt_i <= capt_i + 1;
					if capt_i = 15 then
						armed <= false;
					end if;
				end if;
			else
				-- not armed
				t := debug_mem(to_integer(unsigned(host_bootread_addr(5 downto 2))));
				case host_bootread_addr(1 downto 0) is
					when "00" =>
						host_bootread_data(7 downto 0) <= t(7 downto 0);
					when "01" =>
						host_bootread_data(7 downto 0) <= t(15 downto 8);
					when "10" =>
						host_bootread_data(7 downto 0) <= t(23 downto 16);
					when "11" =>
						host_bootread_data(7 downto 0) <= t(31 downto 24);
					when others =>
						host_bootread_data(7 downto 0) <= t(7 downto 0);
				end case;
			end if;
		end if;
	end process;
	-- end of debug stuff
	
	process(vid_clk)
	begin 
		if rising_edge(vid_clk) then
			tia_divider <= tia_divider + 1;
			if tia_divider = 15 then
				last_hsync <= pre_hsyn;
				last_vsync <= pre_vsyn;
				if tia_hcnt < 160+48 then -- sync this with below
					tia_hcnt <= tia_hcnt + 1;
				end if;
				if pre_hsyn = '1' and last_hsync = '0' then
					tia_hcnt <= (others => '0');
					-- increment vcount on rising edge of hsync.
					-- max 205 lines can be captured into 32K framebuffer
					if tia_vcnt < 205+37 then 	-- sync the offset 37 with below, 20
						tia_vcnt <= tia_vcnt + 1;
					end if;
				end if;
				
				-- not sure if this is correct, but we reset vertical counter when vsync ends
				if last_vsync = '1' and pre_vsyn='0' then
					tia_vcnt <= (others => '0');
				end if;
			end if;
		end if;
	end process;
	
	-- Use a second VGA block to provide timing for HDMI.
	vga_timing_gen : entity work.vga 
		generic map (
			v_input_offset	=> 37,
			h_input_offset	=> 40 -- EP BUGBUG this was 48
		)
		port map (
			I_CLK_VGA	=> clock_vga_s,
			I_COLOR	   => pre_colu,
			I_HCNT		=> tia_hcnt,
			I_VCNT		=> tia_vcnt,
			O_HSYNC		=> hdmi_vga_hsync_n_s, 
			O_VSYNC		=> hdmi_vga_vsync_n_s, 
			O_COLOR		=> hdmi_vga_lum_hue_color_out,
			O_BLANK	   => hdmi_vga_blank_s
		);
		
	-- Secondary colortable for outputting to HDMI
	Inst_HDMI_ColorTable: VGAColorTable PORT MAP(
		clk => 	clock_vga_s,
		lum => 	'0' & hdmi_vga_lum_hue_color_out(2 downto 0),
		hue => 	hdmi_vga_lum_hue_color_out(6 downto 3),
		mode => 	'0' & p_pal,	-- 00 = NTSC, 01 = PAL		-- EP BUGBUG is p_pal high when in PAL?
		outColor => rgb_color
	);			

	hdmi: entity work.hdmi
	generic map (
		FREQ	=> 25000000,	-- pixel clock frequency 
		-- FREQ	=> 25200000,	-- pixel clock frequency 
		FS		=> 48000,		-- audio sample rate - should be 32000, 41000 or 48000 = 48KHz
		CTS	=> 25200,		-- CTS = Freq(pixclk) * N / (128 * Fs)
		N		=> 6144			-- N = 128 * Fs /1000,  128 * Fs /1500 <= N <= 128 * Fs /300 (Check HDMI spec 7.2 for details)
	)
	port map (
		I_CLK_PIXEL		=> clock_vga_s,
		I_R				=> red8,		-- rgb_color(23 downto 16), -- red8,
		I_G				=> green8, 	-- rgb_color(15 downto 8),  -- green8,
		I_B				=> blue8,	-- rgb_color(7 downto 0),   -- blue8,
		I_BLANK			=> hdmi_vga_blank_s,
		I_HSYNC			=> hdmi_vga_hsync_n_s, 
		I_VSYNC			=> hdmi_vga_vsync_n_s,
		-- PCM audio
		I_AUDIO_ENABLE	=> '1',
		I_AUDIO_PCM_L 	=> sound_hdmi_s,
		I_AUDIO_PCM_R	=> sound_hdmi_s,
		-- TMDS parallel pixel synchronous outputs (serialize LSB first)
		O_RED				=> tdms_r_s,
		O_GREEN			=> tdms_g_s,
		O_BLUE			=> tdms_b_s
	);

end architecture;
