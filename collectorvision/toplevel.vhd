--
-- ZXUNO_A2601.vhd
--
-- Atari VCS 2600 toplevel for the Collectorvision
--
-- Modified from ZXUNO version 2016 DistWave
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
		joy2_p1_i			: in    std_logic;
		joy2_p2_i			: in    std_logic;
		joy2_p3_i			: in    std_logic;
		joy2_p4_i			: in    std_logic;
		joy2_p6_i			: in    std_logic;
		joy2_p7_i			: in    std_logic;
		joy2_p9_i			: in    std_logic;
		

-- Audio
      dac_l_o : out std_logic;
      dac_r_o : out std_logic;
		
-- PS2
      ps2_clk_io : inout std_logic;
      ps2_data_io : inout std_logic;

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
  signal vid_clk: std_logic := '0';
  signal ctrl_clk: std_logic := '0';

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
  signal ps2k_clk_out : std_logic;
  signal ps2k_dat_in : std_logic;
  signal ps2k_dat_out : std_logic;

--CtrlModule--
-- Internal video signals:  
  signal vga_vsync_i : std_logic := '0';
  signal vga_hsync_i : std_logic := '0';
  signal vga_red_i : std_logic_vector(7 downto 0) := (others => '0');
  signal vga_green_i : std_logic_vector(7 downto 0) := (others => '0');
  signal vga_blue_i	: std_logic_vector(7 downto 0) := (others => '0');
  
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
	signal P_L: std_logic;
	signal P_R: std_logic;
	signal P_A: std_logic;
	signal P_U: std_logic;
	signal P_D: std_logic;
-- EP fake HDMI output, not functional
	signal clock_vga_s		: std_logic;
	signal clock_hdmi_s		: std_logic;
	signal clock_hdmi_n_s	: std_logic;

	signal tdms_r_s			: std_logic_vector( 9 downto 0) := (others => '0');
	signal tdms_g_s			: std_logic_vector( 9 downto 0) := (others => '0');
	signal tdms_b_s			: std_logic_vector( 9 downto 0) := (others => '0');
	
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

begin

	ps2k_dat_in <= ps2_data_io;
	ps2_data_io <= '0' when ps2k_dat_out='0' else 'Z';
	ps2k_clk_in <= ps2_clk_io;
	ps2_clk_io  <= '0' when ps2k_clk_out='0' else 'Z';

	ps2k_clk_out <= '1';
	ps2k_dat_out <= '1';

-- Serial flash - not used right now
	flash_cs_n_o <= '1';
	flash_sclk_o <= '0';
	flash_mosi_o <= '0';
	flash_wp_o   <= '0';
	flash_hold_o <= '0';
		
-- SRAM - not used	
--	sram_oe_n_o	<= '1';
--	sram_we_n_o <= '1';
--	sram_data_io <= "ZZZZZZZZ";
--	sram_addr_o <= (others => '0');
	
-- HDMI not used. A completely non-functional piece of code to keep synthesis process happy.
--		by using actual IO drivers.
	clock_vga_s <= vid_clk;
	clock_hdmi_s <= vid_clk;
	clock_hdmi_n_s <= not vid_clk;
	
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
	joy_p8_o <= '0';
	joy_p5_o <= '0';
	
	P_L	<= joy1_p3_i;
	P_R	<= joy1_p4_i;
	P_A 	<= joy1_p6_i;
	P_U 	<= joy1_p1_i;
   P_D 	<= joy1_p2_i;
-- Control module

MyCtrlModule : entity work.CtrlModule
	port map (
		clk => ctrl_clk,
		reset_n => '1',

		-- Video signals for OSD
		vga_hsync => vga_hsync_i,
		vga_vsync => vga_vsync_i,
		osd_window => osd_window,
		osd_pixel => osd_pixel,

		-- PS2 keyboard
		ps2k_clk_in => ps2k_clk_in,
		ps2k_dat_in => ps2k_dat_in,
		
		-- SD card signals
		spi_clk => sd_sclk_o,
		spi_mosi => sd_mosi_o,
		spi_miso => sd_miso_i,
		spi_cs => sd_cs_n_o,
		
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

overlay : entity work.OSD_Overlay
	port map
	(
		clk => ctrl_clk,
		red_in => vga_red_i,
		green_in => vga_green_i,
		blue_in => vga_blue_i,
		window_in => '1',
		osd_window_in => osd_window,
		osd_pixel_in => osd_pixel,
		hsync_in => vga_hsync_i,
		red_out(7 downto 4) => vga_r_o,
		red_out(3 downto 0) => open,
		green_out(7 downto 4) => vga_g_o,
		green_out(3 downto 0) => open,
		blue_out(7 downto 4) => vga_b_o,
		blue_out(3 downto 0) => open,
		window_out => open,
		scanline_ena => scanlines
	);

  -- SRAM_nWE <= '1'; -- disable ram

  vga_hsync_n_o <= vga_hsync_i;
  vga_vsync_n_o <= vga_vsync_i;
 
--  NTSC <= '0';
--  PAL <= '0';

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
			clk_i => ctrl_clk,
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
		ram_clk => ctrl_clk,
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
  pllInstance : entity work.pll
    port map (
      CLK_IN1 => CLOCK_50_i,
      CLK_OUT1 => vid_clk,
		CLK_OUT2 => ctrl_clk
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

end architecture;
