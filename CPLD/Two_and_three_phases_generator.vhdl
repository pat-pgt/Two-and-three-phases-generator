library ieee;
use ieee.std_logic_1164.all, ieee.numeric_std.all, ieee.math_real.all;

package Two_and_three_phases_generator_CPLD is
  
  component multiphases_counter is
    generic(
      sinus_not_triangle : boolean := false;
      count_size         : positive;
      clk_frequency      : positive;
      output_frequency   : positive
      );
    port(
      CLK  : in  std_logic;
      EN   : in  std_logic;
      PBT1 : out std_logic_vector;
      PB2  : out std_logic_vector;
      PT2  : out std_logic_vector;
      PT3  : out std_logic_vector);

  end component multiphases_counter;

  component DAC_lowcost is
    generic(
      start_bits : positive := 2);
    port(
      the_input  : in  std_logic_vector;
      the_output : out std_logic_vector);
  end component DAC_lowcost;

  function populate_sinus_step_table(step : positive; counter_length : positive; y_size : positive) return integer;
  
end package Two_and_three_phases_generator_CPLD;

package body Two_and_three_phases_generator_CPLD is
  function populate_sinus_step_table(step : positive; counter_length : positive; y_size : positive) return integer is
    variable X : real;
  begin
    X := arcsin(real(step) / real(y_size + 1));
    -- Normalise to one quadrant
    X := X * 2.0 / math_pi;
    -- Normalise to X
    X := X * real(2 ** counter_length / 4);
    return integer(floor(X));
  end function populate_sinus_step_table;
end package body Two_and_three_phases_generator_CPLD;

library ieee;
use ieee.std_logic_1164.all, ieee.numeric_std.all, ieee.math_real.all, work.Two_and_three_phases_generator_CPLD.all;

--! Low cost digital to analogue converter
--!
--! This is for low number of bits. Its advantage is the sourcing is always guarantee
--!   as resistors or resistors networks can be found easily.\n
--! However, a special care should be taken as the CPLD outputs
--!   are never guarantee to be rail to rail.
--! To fix this problem, more than one output pin is used per bit.\n
--! With many available output pins, one can set:
--!   4 for the high bit
--!   2 for the high - 1 bit
--!   1 for the high - 2 bit
--! By this way
--!   * all the output has the same voltage close to the rails
--!     especially with values around 10K.
--!   * the resistors have the same value, and a network can be used.
--!     Basic 5% resistors can be used.\n
--! Outputs with 4, 5 or 6 bits has to use low bits resistors by multiples
--!   or set more resistors in series.
--!   The high bits uses 0.05% differential, 2% absolute networks,
--!   The low bits can use 7 independent resistors as
--!     4 in series, 2 in series and 1.\n
entity DAC_lowcost is
  generic(
    start_bits : positive := 2);
  port(
    the_input  : in  std_logic_vector;
    the_output : out std_logic_vector);
end entity DAC_lowcost;

architecture rtl of DAC_lowcost is
begin

  main_proc : process(the_input)
    variable nbre_bits_power_2   : natural;
    variable output_highbit_iter : integer;
  begin
    nbre_bits_power_2   := start_bits;
    output_highbit_iter := the_output'high;
    bits_dispatch : for ind in the_input'high downto the_input'low loop
      the_output(output_highbit_iter downto output_highbit_iter - 2 ** nbre_bits_power_2 + 1) <=
        (others => the_input(ind));
      output_highbit_iter :=
        output_highbit_iter - 2 ** nbre_bits_power_2;
      if nbre_bits_power_2 > 0 then
        nbre_bits_power_2 := nbre_bits_power_2 - 1;
      end if;
    end loop bits_dispatch;
  end process main_proc;
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all, ieee.numeric_std.all, ieee.math_real.all, work.Two_and_three_phases_generator_CPLD.all;

--! Generates bi and triphase signals
--!
--! It is intended for a low number of output bits and a high master clock.\n
--! The goal is to use low cost and long term converter with resistors
--! connected on the (output) pins.\n
--! The counter describing a full rotation should be as large as possible,
--!   it increase the phase precision as a power of 2 can never be divided by 3,
--!   however, a frequency error occurs if the master clock is not a multiple (see below)
--! 
entity multiphases_counter is
  generic(
    sinus_not_triangle : boolean := false;
    --! Size of the counter as a power of 2
    count_size         : positive;
    --! The unit is not important as long as the output frequency uses the same
    clk_frequency      : positive;
    --! The unit is not important as long as the master clock uses the same
    output_frequency   : positive
    );
  port(
    CLK  : in  std_logic;
    EN   : in  std_logic;
    --! reference signal
    PBT1 : out std_logic_vector;
    --! gets a 2 phases with the reference
    PB2  : out std_logic_vector;
    --! gets a 3 phases with PT3 and the reference
    PT2  : out std_logic_vector;
    --! gets a 3 phases with PT2 and the reference
    PT3  : out std_logic_vector);

end entity multiphases_counter;

--! Architecture RTL of multiphases_counter
--!
--! Since the master clock is high the the number of bits is low,
--!   we are not proceeding by calculating the value at each time,
--!   but we populate a table of the times in which the output changes.\n
--! To avoid excessive resources consumption, only one signal calculator is instantiated.
--! The output buffer is set with the result and the counter spins by 90 degrees.
--! the PB2 buffer is set with the result and the counter spins by 30 degrees.
--! the PT2 buffer is set with the result and the counter spins by 120 degrees.
--! the PT3 buffer is set with the result and the counter spins by 120 degrees
--!   plus and epsilon.\n
--! It is designed that the sum of all additions add 2**<counter size> + 1.
--! Then, the counter is incremented by 1 after all the 4 signals.
--! A clock of required frequency * 2**<counter size> * 4
--!   is generated by a division by N from the master clock.\n
--! That generates a little phase error.
--! If the phase is critics, one can add 4 buffers at the output,
--!   but enlarging the counter reduces the phase error as well.
architecture rtl of multiphases_counter is
  constant prediv_ratio          : positive                                   := clk_frequency / (2** count_size * output_frequency * 4);
  constant prediv_size           : positive                                   := 10;
  signal prediv, prediv_next     : std_logic_vector(prediv_size - 1 downto 0);
  constant prediv_max            : std_logic_vector(prediv_size - 1 downto 0) := std_logic_vector(to_unsigned(prediv_ratio - 1, prediv_size));
  signal selector, selector_next : std_logic_vector(1 downto 0);
  signal counter                 : std_logic_vector(count_size - 1 downto 0);
  signal counter_next            : std_logic_vector(count_size - 1 downto 0);
  signal PBT1_s, PBT1_s_next     : std_logic_vector(PBT1'range);
  signal PB2_s, PB2_s_next       : std_logic_vector(PB2'range);
  signal PT2_s, PT2_s_next       : std_logic_vector(PT2'range);
  signal PT3_s, PT3_s_next       : std_logic_vector(PT3'range);
  constant sinus_step_size       : integer                                    := 2**(PBT1'length - 1) - 1;
  type sinus_step_t is array (sinus_step_size downto 1) of unsigned(counter'length - 3 downto 0);
  signal sinus_step              : sinus_step_t;
begin
  assert count_size > 2 report "The system needs at least 3 bits of counter" severity failure;
  assert false report "Phase angle (between phases) precision due to the count step is " & real'image(360.0 / real(2**count_size)) & " degree" severity note;
  assert false report "Phase angle (between phases) precision due to the sequential process is " & real'image(1080.0 * real(output_frequency) / real(clk_frequency)) & " degree" severity note;
  assert prediv_ratio > 0 report "Count_size too big or CLK frequency too low, CLK should be at least " & integer'image(2** count_size * output_frequency * 4)
    severity failure;
  assert PBT1'length = PB2'length
    report "Size of all the outputs should be the same (" & integer'image(PBT1'length) & " vs " & integer'image(PB2'length) & ")"
    severity failure;
  assert PBT1'length = PT2'length report "Size of all the outputs should be the same ("& integer'image(PBT1'length) & " vs " & integer'image(PB2'length) & ")"
    severity failure;
  assert PBT1'length = PT3'length report "Size of all the outputs should be the same"
    severity failure;
  assert PBT1'length > 1 report "Size of all the outputs should be at least 2 bits"
    severity failure;

  assert PBT1'length - 1 >= count_size - 2 or sinus_not_triangle
    report "Triangle rendering, not all the output precision in amplitude is used ( " & integer'image(count_size - 1 - PBT1'length) & " bits)"
    severity note;
  assert PBT1'length - 1 <= count_size - 2 or sinus_not_triangle
                            report "Triangle rendering, the " & integer'image(PBT1'length - count_size + 1) & " low bits of the outputs are irrelevant"
                            severity warning;
  assert clk_frequency / output_frequency > 4 * 2 ** (PBT1'length - 1) or sinus_not_triangle
    report "For good triangle rendering, the steps should contain at least one clock cycles" severity warning;

  -- Claim: If the first element is not 0, then 2 consecutive elements are
  -- always different
  assert populate_sinus_step_table(1, counter'length, sinus_step_size) /= 0
    report "in sinus mode, the output size should not be too big to make this algorithm up and running"
    severity error;
  
  assert false report "TODO: Make prediv_size dynamic" severity warning;

  sinustable : if sinus_not_triangle generate
    sinuselem : for i in 1 to sinus_step_size generate
    begin
      sinus_step(i) <= to_unsigned(populate_sinus_step_table(i, counter'length, sinus_step_size), counter'length - 2);

      assert false report "Step " & integer'image(populate_sinus_step_table(i, counter'length, sinus_step_size)) severity note;
    end generate sinuselem;
  end generate sinustable;

  PBT1 <= PBT1_s;
  PB2  <= PB2_s;
  PT2  <= PT2_s;
  PT3  <= PT3_s;

  comb : process(EN, PB2_s, PBT1_s, PT2_s, PT3_s, counter, prediv, selector)
    variable conversion_input  : std_logic_vector(counter'length - 3 downto 0);
    variable output_candidate2 : std_logic_vector(PBT1'range);
    variable output_quadrant   : std_logic_vector(1 downto 0);
  begin
    if EN = '1' then
      -- Computes one output.\n
      -- Since the triangles and the sine have many symmetries
      --   Only 1/4 is computed.
      -- If the high bit is 1, it is void and the result is negated.
      --   There is not one addition (as the 2'nd complement required)
      --   because the values are, in fact, for 3 bits, 0.5, 1.5, 2.5, 3.5
      --   which can be inverted into -0.5, -1.5, -2.5, -3.5
      -- If the high - 1 bit is 1, the lowest bits are inverted.
      -- If both the high and the high - 1 are 1, both rules above applies.
      if prediv = prediv_max or prediv_ratio = 0 then
        conversion_input := counter(counter'high - 2 downto counter'low);
        output_quadrant  := counter(counter'high downto counter'high - 1);
        novhdl2008_1 : for i in conversion_input'high downto conversion_input'low loop
          conversion_input(i) := output_quadrant(0) xor conversion_input(i);
        end loop novhdl2008_1;
        if sinus_not_triangle then
          -- Search the table to find the relevant threshold\n
          -- This part should be improved to  check sequentially rather than in parallel
          output_candidate2(output_candidate2'high - 1 downto output_candidate2'low) := (others => '1');
          for i in sinus_step'range loop
            if unsigned(sinus_step(i)) < unsigned(conversion_input) then
              exit;
            else
              output_candidate2(output_candidate2'high - 1 downto output_candidate2'low) :=
                std_logic_vector(unsigned(output_candidate2(output_candidate2'high - 1 downto output_candidate2'low)) -1);
            end if;
          end loop;
        else
          -- nothing to do except copy conversion_input to output candidate 2
          novhdl2008_2 : for i in output_candidate2'high - 1 downto output_candidate2'low loop
            if i - output_candidate2'high + 1 + conversion_input'high >= conversion_input'low then
              output_candidate2(i) := conversion_input(i - output_candidate2'high + 1 + conversion_input'high);
            else
              output_candidate2(i) := '0';
            end if;
          end loop novhdl2008_2;
        end if;
        -- do the one quadrant conversion triangle sinus here on the length - 2 low bits
        output_candidate2(output_candidate2'high) := not output_quadrant(1);
        novhdl2008_3 : for i in output_candidate2'high - 1 downto output_candidate2'low loop
          output_candidate2(i) := output_quadrant(1) xor output_candidate2(i);
        end loop novhdl2008_3;

        -- Now, we take the output to store into the output register
        --   and we add a constant to spin it (see details below)
        case selector is
          when "00" =>
            selector_next <= "01";
            -- The most easy, without any rounding error + 90 degrees
            counter_next  <= std_logic_vector(unsigned(counter) + to_unsigned(2 ** (count_size - 2), count_size));
            PBT1_s_next   <= output_candidate2;
            PB2_s_next    <= PB2_s;
            PT2_s_next    <= PT2_s;
            PT3_s_next    <= PT3_s;
          when "01" =>
            selector_next <= "10";
            counter_next  <= std_logic_vector(unsigned(counter) + to_unsigned(2 ** count_size / 3 - 2 ** (count_size - 2), count_size));
            PBT1_s_next   <= PBT1_s;
            PB2_s_next    <= output_candidate2;
            PT2_s_next    <= PT2_s;
            PT3_s_next    <= PT3_s;
          when "10" =>
            selector_next <= "11";
            -- We add 1 to compensate the rounding of the division by 3
            -- by this and the previous case
            counter_next  <= std_logic_vector(unsigned(counter) + to_unsigned(2 ** count_size / 3 + 1, count_size));
            PBT1_s_next   <= PBT1_s;
            PB2_s_next    <= PB2_s;
            PT2_s_next    <= output_candidate2;
            PT3_s_next    <= PT3_s;
          when "11" =>
            selector_next <= "00";
            -- We add 1 to increment after spinning completely 
            counter_next  <= std_logic_vector(unsigned(counter) + to_unsigned(2 ** count_size / 3 + 1, count_size));
            PBT1_s_next   <= PBT1_s;
            PB2_s_next    <= PB2_s;
            PT2_s_next    <= PT2_s;
            PT3_s_next    <= output_candidate2;
          when others => null;
        end case;
        prediv_next <= (others => '0');
      else
        PBT1_s_next   <= PBT1_s;
        PB2_s_next    <= PB2_s;
        PT2_s_next    <= PT2_s;
        PT3_s_next    <= PT3_s;
        prediv_next   <= std_logic_vector(unsigned(prediv) + 1);
        selector_next <= selector;
        counter_next  <= counter;
      end if;
    else
      counter_next  <= (others => '0');
      prediv_next   <= (others => '0');
      selector_next <= (others => '0');
    end if;
  end process comb;

  latch : process (CLK) is
  begin
    if rising_edge(CLK) then
      PBT1_s   <= PBT1_s_next;
      PB2_s    <= PB2_s_next;
      PT2_s    <= PT2_s_next;
      PT3_s    <= PT3_s_next;
      selector <= selector_next;
      counter  <= counter_next;
      prediv   <= prediv_next;
    end if;
  end process latch;

end architecture rtl;

