-- ram2048x8 memory for E7 banking mode support
-- EP 2019-05-02
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram2048x8 is
    port(clk: in std_logic;
         r: in std_logic;
         d_in: in std_logic_vector(7 downto 0);
         d_out: out std_logic_vector(7 downto 0);
         a: in std_logic_vector(10 downto 0));
end ram2048x8;

architecture arch of ram2048x8 is
    type ram_type is array (0 to 2047) of
        std_logic_vector(7 downto 0);
    signal ram: ram_type;
begin

    process (clk, r, a)
    begin
        if (clk'event and clk = '1') then
            if (r = '1') then
                d_out <= ram(to_integer(unsigned(a)));
            else
                ram(to_integer(unsigned(a))) <= d_in;
            end if;
        end if;
    end process;

end arch;