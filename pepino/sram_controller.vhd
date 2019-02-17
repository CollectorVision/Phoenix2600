----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 			Erik Piehl
-- 
-- Create Date:    18:32:49 10/28/2018 
-- Design Name: 	A2600 external SRAM controller for the collector vision board
-- Module Name:    sram_controller - Behavioral_sram_controller 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.numeric_std.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity sram_controller is
    Port ( 	sram_addr_o  : out  STD_LOGIC_VECTOR (20 downto 0);
           	sram_data_io : inout  STD_LOGIC_VECTOR (7 downto 0);
           	sram_oe_n_o  : out  STD_LOGIC;
           	sram_we_n_o  : out  STD_LOGIC;
           	clk_i 			: in  STD_LOGIC;
				reset_i		: in STD_LOGIC;
				
				-- read data (debugging interface)
				host_bootread_data 	: out std_logic_vector(31 downto 0);
				host_bootread_addr 	: in std_logic_vector(15 downto 0);
				host_bootread_req 	: in std_logic;
				host_bootread_ack 	: out std_logic := '0';
				
				-- write data
           	host_bootdata : in  STD_LOGIC_VECTOR (31 downto 0);
           	host_bootdata_req : in  STD_LOGIC;
           	host_bootdata_ack : out  STD_LOGIC;
				rom_loaded_o	: out STD_LOGIC;
           	a2600_addr_i : in  STD_LOGIC_VECTOR (15 downto 0);
           	a2600_data_o : out  STD_LOGIC_VECTOR (7 downto 0));
end sram_controller;

architecture Behavioral_sram_controller of sram_controller is

	type boot_states is (idle, ramwait, 
		write_boot_byte0, write_boot_byte1, write_boot_byte2,
		write_byte_strobe0, write_byte_strobe1,
		read_host_byte0, read_host_byte1, read_host_byte2, read_host_byte3,
		grace,
		read_2600_byte0, read_2600_byte1 );
	signal boot_state 	 	: boot_states := idle;
	signal return_state     : boot_states := idle;	-- for subroutines our "return address"
	signal last_a2600_addr  : std_logic_vector(15 downto 0);
	signal a2600_databyte   : std_logic_vector(7 downto 0);
	signal ram_addr 		  	: std_logic_vector(12 downto 0) := "0000000000000"; -- 12 32k
	signal rom_loaded			: std_logic := '0';
	signal host_read_buf    : std_logic_vector(31 downto 0) := x"98765432";

begin

	-- This state machine is from the file A2601NoFlash.vhd originally,
	-- but modifed heavily since the use case with external SRAM is very different.
	-- State machine to receive and stash boot data in SRAM
	
	a2600_data_o <= a2600_databyte;
	rom_loaded_o <= rom_loaded;
	host_bootread_data <= host_read_buf;
	
	process(clk_i, host_bootdata_req, a2600_addr_i)
	begin
		if rising_edge(clk_i) then
			if reset_i='1' then
				ram_addr <= "0000000000000"; -- "0000000000";
				host_bootdata_ack <= '0';
				boot_state<=idle;
				last_a2600_addr <= (others => '0');
				sram_data_io <= "ZZZZZZZZ";
				host_bootread_ack <= '0';
			else
				host_bootdata_ack <= '0';
				host_bootread_ack <= '0';
				case boot_state is
					when idle =>
						if last_a2600_addr /= a2600_addr_i then
							boot_state 		<= read_2600_byte0;
							last_a2600_addr <= a2600_addr_i;
							
							sram_data_io 	<= "ZZZZZZZZ";
							sram_addr_o 	<= "00000" & a2600_addr_i;
							sram_oe_n_o 	<= '0';
							sram_we_n_o 	<= '1';
							
						elsif host_bootdata_req ='1' then
							boot_state		<= write_byte_strobe0;
							return_state	<= write_boot_byte0;	-- continue from here
							sram_data_io 	<= host_bootdata(7 downto 0);
							sram_addr_o 	<= "000000" & ram_addr & "11";
							sram_oe_n_o 	<= '1';
							
						elsif host_bootread_req = '1' then
							boot_state 		<= read_host_byte0;
							sram_addr_o    <= "00000" & host_bootread_addr(15 downto 2) & "11";
							sram_data_io 	<= "ZZZZZZZZ";
							sram_oe_n_o		<= '0';
							sram_we_n_o    <= '1';
						end if;
					when ramwait =>
						ram_addr <= std_logic_vector((unsigned(ram_addr)+1));
						host_bootdata_ack <= '1';							
						boot_state <= grace;
						rom_loaded <= '1';
					when grace =>
						boot_state <= idle;
						
					when write_boot_byte0 =>
						sram_data_io 	<= host_bootdata(15 downto 8);
						sram_addr_o(1 downto 0) <= "10";
						boot_state		<= write_byte_strobe0;
						return_state 	<= write_boot_byte1;
					when write_boot_byte1 =>
						sram_data_io 	<= host_bootdata(23 downto 16);
						sram_addr_o(1 downto 0) <= "01";
						boot_state		<= write_byte_strobe0;
						return_state 	<= write_boot_byte2;
					when write_boot_byte2 =>
						sram_data_io 	<= host_bootdata(31 downto 24);
						sram_addr_o(1 downto 0) <= "00";
						boot_state 		<= write_byte_strobe0;
						return_state 	<= ramwait;

					-- the following two states issue a write pulse; giving data & addr time to settle
					when write_byte_strobe0 =>
						sram_we_n_o 	<= '0';
						boot_state 		<= write_byte_strobe1;
					when write_byte_strobe1 =>
						sram_we_n_o 	<= '1';
						sram_data_io 	<= "ZZZZZZZZ";
						boot_state 		<= return_state;
						
					when read_host_byte0 =>
						host_read_buf(7 downto 0) <= sram_data_io;
						sram_addr_o(1 downto 0) <= "10";
						boot_state <= read_host_byte1;
					when read_host_byte1 =>
						host_read_buf(15 downto 8) <= sram_data_io;
						sram_addr_o(1 downto 0) <= "01";
						boot_state <= read_host_byte2;
					when read_host_byte2 =>
						host_read_buf(23 downto 16) <= sram_data_io;
						sram_addr_o(1 downto 0) <= "00";
						boot_state <= read_host_byte3;
					when read_host_byte3 =>
						host_read_buf(31 downto 24) <= sram_data_io;
						sram_oe_n_o <= '1';
						boot_state <= idle;
						host_bootread_ack <= '1';

					when read_2600_byte0 =>
						boot_state <=  read_2600_byte1;	-- waste 1 clock cycle
					when read_2600_byte1 =>
						boot_state <= idle;
						a2600_databyte <= sram_data_io;
						sram_oe_n_o <= '1';
						sram_we_n_o <= '1';
						
				end case;
			end if;
		end if;
	end process;

end Behavioral_sram_controller;

