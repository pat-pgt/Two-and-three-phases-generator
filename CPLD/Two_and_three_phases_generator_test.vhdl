library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Two_and_three_phases_generator_CPLD.all;


entity Two_and_three_phases_generator_CPLD_test is
end entity Two_and_three_phases_generator_CPLD_test;

architecture arch of Two_and_three_phases_generator_CPLD_test is
  signal EN1, EN2, EN3      : std_logic := '0';
  signal CLK                : std_logic := '0';
  signal PBT1               : std_logic_vector(13 downto 10);
  signal PB2                : std_logic_vector(13 downto 10);
  signal PT2                : std_logic_vector(13 downto 10);
  signal PT3                : std_logic_vector(13 downto 10);
  constant count_size       : positive  := 8;
  constant clk_frequency    : positive  := 2000000;
  constant output_frequency : positive  := 400;
  constant simul_ratio      : positive  := 2 * clk_frequency / output_frequency;
  constant simul_size       : positive  := 20;

  signal simul       : unsigned(simul_size downto 1) := (others => '0');
  constant simul_max : unsigned(simul_size downto 1) := to_unsigned(simul_ratio + 1, simul_size);

  signal output_lowcost : std_logic_vector(25 downto 11);
begin

  instanc : multiphases_counter
    generic map (
      sinus_not_triangle => true,
      count_size         => count_size,
      clk_frequency      => clk_frequency,
      output_frequency   => output_frequency)
    port map (
      CLK  => CLK,
      EN   => EN3,
      PBT1 => PBT1,
      PB2  => PB2,
      PT2  => PT2,
      PT3  => PT3); 

  lowcost_DAC_PBT1 : DAC_lowcost
    generic map(
      start_bits => 3)
    port map(
      the_input  => PBT1,
      the_output => output_lowcost);

  main_proc : process
    constant time_wait : real := (real(1) / (real(clk_frequency) * 2.0));
  begin
    CLK <= not CLK;
    if simul /= simul_max then
      if CLK = '0' then
        EN3   <= EN2;
        EN2   <= EN1;
        EN1   <= '1';
        simul <= simul + 1;
      end if;
      wait for 125 ns;
    else
      wait;
    end if;
  end process main_proc;

end architecture arch;
