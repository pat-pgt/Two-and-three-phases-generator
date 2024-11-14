
library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all;


entity Two_phases_generator is
  port (
    CLK : in  std_logic;
    RST : in  std_logic;
    --! Output phase 1. The size is fixed by construction
    PB1 : out std_logic_vector(1 downto 0);
    --! Output phase 2. The size is fixed by construction
    PB2 : out std_logic_vector(1 downto 0));
end entity Two_phases_generator;

architecture RTL of Two_phases_generator is
  signal the_counter : std_logic_vector(2 downto 0);
begin
  main_proc : process (CLK) is
    variable xor_2_high_bits : std_logic;
  begin
    if rising_edge(CLK) then
      RST_if : if RST = '1' then
        the_counter     <= std_logic_vector(unsigned(the_counter) + 1);
        -- Yes, we are taking the last state of the counter.
        -- However, the counter is only internal,
        -- and no phase relation has been required with any external signal.
        xor_2_high_bits := the_counter(2) xor the_counter(1);
        PB1(1)          <= not the_counter(2);
        PB1(0)          <= xor_2_high_bits xor the_counter(0);
        PB2(1)          <= xor_2_high_bits;
        PB2(0)          <= the_counter(2) xor the_counter(0);
      end if RST_if;
    end if;
  end process main_proc;
end architecture RTL;
