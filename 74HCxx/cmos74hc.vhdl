library IEEE;
use IEEE.Std_Logic_1164.all;

package cmos74hc is
  component hc7486
    port(
      P1, P2 : in std_logic;
      P3 : out std_logic ;
      P4, P5 : in std_logic;
      P6 : out std_logic;
      P9, P10 : in std_logic;
      P8 : out std_logic ;
      P12, P13 : in std_logic;
      P11 : out std_logic;
      P7 : inout std_logic;
      P14 : inout std_logic);
  end component hc7486;
  component hc7400
    port(
      P1, P2 : in std_logic;
      P3 : out std_logic ;
      P4, P5 : in std_logic;
      P6 : out std_logic;
      P9, P10 : in std_logic;
      P8 : out std_logic ;
      P12, P13 : in std_logic;
      P11 : out std_logic;
      P7 : inout std_logic;
      P14 : inout std_logic);
  end component hc7400;
  component hc7420
    port(
      P1, P2 : in std_logic;
      P4, P5 : in std_logic;
      P6 : out std_logic;
      P9, P10 : in std_logic;
      P8 : out std_logic ;
      P12, P13 : in std_logic;
      P7 : inout std_logic;
      P14 : inout std_logic);
  end component hc7420;
  component hc7485 is
    port (
      P1 : in std_logic;
      P2, P3, P4 : in std_logic;
      P5, P6, P7 : out std_logic;
      P9, P10, P11, P12, P13, P14, P15 : in std_logic;
      P8 : inout std_logic;
      P16 : inout std_logic);
  end component hc7485;
  component hc7473
    port(
      P1, P2, P3 : in std_logic;
      P5, P6, P7 : in std_logic;
      P8, P9 : out std_logic;
      P10 : in std_logic;
      P12, P13 : out std_logic;
      P14 : in std_logic;
      P4 : inout std_logic;
      P11 : inout std_logic);
  end component hc7473;
  component hc74163
    generic (
      Initial_value : std_logic_vector( 3 downto 0 ) := "0110"
      );
    port (
      P1 : in  std_logic;
      P2 : in  std_logic;
      P3 : in  std_logic;
      P4 : in  std_logic;
      P5 : in  std_logic;
      P6 : in  std_logic;
      P7 : in  std_logic;
      P9 : in  std_logic;
      P10 : in  std_logic;
      P11 : out std_logic;
      P12 : out std_logic;
      P13 : out std_logic;
      P14 : out std_logic;
      P15 : out std_logic;
      P8  : inout std_logic;
      P16 : inout std_logic
      );
  end component hc74163;
  component hc74283
    port (
      P1 : out std_logic;
      P2 : in  std_logic;
      P3 : in  std_logic;
      P4 : out  std_logic;
      P5 : in  std_logic;
      P6 : in  std_logic;
      P7 : in  std_logic;
      P9 : out  std_logic;
      P10 : out  std_logic;
      P11 : in std_logic;
      P12 : in std_logic;
      P13 : out std_logic;
      P14 : in std_logic;
      P15 : in std_logic;
      P8  : inout std_logic;
      P16 : inout std_logic
      );
  end component hc74283;
  component hc74273
    generic (
      Initial_value : std_logic_vector( 7 downto 0 ) := "00110100");
    port (
      P1 : in  std_logic;
      P2 : out  std_logic;
      P3 : in  std_logic;
      P4 : in  std_logic;
      P5 : out  std_logic;
      P6 : out  std_logic;
      P7 : in  std_logic;
      P8 : in  std_logic;
      P9 : out  std_logic;
      P11 : in std_logic;
      P12 : out std_logic;
      P13 : in std_logic;
      P14 : in std_logic;
      P15 : out std_logic;
      P16 : out std_logic;
      P17 : in std_logic;
      P18 : in std_logic;
      P19 : out std_logic;
      P10  : inout std_logic;
      P20 : inout std_logic
      );
  end component hc74273;
  component hc74138
    port (
      P1 : in  std_logic;
      P2 : in  std_logic;
      P3 : in  std_logic;
      P4 : in  std_logic;
      P5 : in  std_logic;
      P6 : in  std_logic;
      P7 : out  std_logic;
      P9 : out  std_logic;
      P10 : out  std_logic;
      P11 : out std_logic;
      P12 : out std_logic;
      P13 : out std_logic;
      P14 : out std_logic;
      P15 : out std_logic;
      P8  : inout std_logic;
      P16 : inout std_logic
      );
  end component hc74138;
end package cmos74hc;

library IEEE;
use IEEE.Std_Logic_1164.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc7486 is
  port (
    P1, P2 : in std_logic;
    P3 : out std_logic ;
    P4, P5 : in std_logic;
    P6 : out std_logic;
    P9, P10 : in std_logic;
    P8 : out std_logic ;
    P12, P13 : in std_logic;
    P11 : out std_logic;
    P7 : inout std_logic;
    P14 : inout std_logic);
end entity hc7486;
-- Context clause


architecture rtl of hc7486 is
begin
  -- assert P7 /= '0' report "A hc86 does not have his ground connected" severity failure;
  -- assert P14 /= '1' report "A hc86 does not have his vcc connected" severity failure;  
  P3 <= P1 xor P2;
  P6 <= P4 xor P5;
  P8 <= P9 xor P10;
  P11 <= P13 xor P12;
end architecture rtl;


library IEEE;
use IEEE.Std_Logic_1164.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc7400 is
  port (
    P1, P2 : in std_logic;
    P3 : out std_logic ;
    P4, P5 : in std_logic;
    P6 : out std_logic;
    P9, P10 : in std_logic;
    P8 : out std_logic ;
    P12, P13 : in std_logic;
    P11 : out std_logic;
    P7 : inout std_logic;
    P14 : inout std_logic);
end entity hc7400;
-- Context clause


architecture rtl of hc7400 is
begin
  -- assert P7 /= '0' report "A hc00 does not have his ground connected" severity failure;
  -- assert P14 /= '1' report "A hc00 does not have his vcc connected" severity failure;  
  P3 <= P1 nand P2;
  P6 <= P4 nand P5;
  P8 <= P9 nand P10;
  P11 <= P13 nand P12;
end architecture rtl;


library IEEE;
use IEEE.Std_Logic_1164.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc7420 is
  port (
    P1, P2 : in std_logic;
    P4, P5 : in std_logic;
    P6 : out std_logic;
    P9, P10 : in std_logic;
    P8 : out std_logic ;
    P12, P13 : in std_logic;
    P7 : inout std_logic;
    P14 : inout std_logic);
end entity hc7420;
-- Context clause


architecture rtl of hc7420 is
begin
  -- assert P7 /= '0' report "A hc00 does not have his ground connected" severity failure;
  -- assert P14 /= '1' report "A hc00 does not have his vcc connected" severity failure;  
  P6 <= not ( P1 and P2 and P4 and P5 );
  P8 <= not ( P9 and P10 and P12 and P13 );
end architecture rtl;

library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc7485 is
  port (
    P1 : in std_logic;
    P2, P3, P4 : in std_logic;
    P5, P6, P7 : out std_logic;
    P9, P10, P11, P12, P13, P14, P15 : in std_logic;
    P8 : inout std_logic;
    P16 : inout std_logic);
end entity hc7485;
-- Context clause


architecture rtl of hc7485 is
  signal A : std_logic_vector( 3 downto 0 );
  signal B : std_logic_vector( 3 downto 0 );
begin
  -- assert P7 /= '0' report "A hc00 does not have his ground connected" severity failure;
  -- assert P14 /= '1' report "A hc00 does not have his vcc connected" severity failure;  
  A( 0 ) <= P10;
  A( 1 ) <= P12;
  A( 2 ) <= P13;
  A( 3 ) <= P15;
  B( 0 ) <= P9;
  B( 1 ) <= P11;
  B( 2 ) <= P14;
  B( 3 ) <= P1;

  main_proc : process ( A, B, P2, P3, P4 )
    begin
      if unsigned( A ) /= unsigned( B ) then
        if unsigned( A ) > unsigned ( B ) then
          P5 <= '1';
          P6 <= '0';
          P7 <= '0';
        else
          P5 <= '0';
          P6 <= '0';
          P7 <= '1';
        end if;
      else
        if P3 = '1' then
          P5 <= '0';
          P6 <= '1';
          P7 <= '0';
        else
          P5 <= not P2;
          P6 <= '0';
          P7 <= not P4;
        end if;
      end if;
    end process main_proc;
end architecture rtl;

library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

entity hc7473 is
  port(
    P1, P2, P3 : in std_logic;
    P5, P6, P7 : in std_logic;
    P8, P9 : out std_logic;
    P10 : in std_logic;
    P12, P13 : out std_logic;
    P14 : in std_logic;
    P4 : inout std_logic;
    P11 : inout std_logic);
end entity hc7473;

architecture rtl of hc7473 is
  signal JK1 : std_logic_vector( 0 to 1 );
  signal QQB1 : std_logic := '1';
  signal JK2 : std_logic_vector( 0 to 1 );
  signal QQB2 : std_logic := '0';
begin
  -- assert P4 /= '0' report "A hc00 does not have his ground connected" severity failure;
  -- assert P11 /= '1' report "A hc00 does not have his vcc connected" severity failure;  
  JK1( 0 ) <= P14;
  JK1( 1 ) <= P3;
  JK2( 0 ) <= P7;
  JK2( 1 ) <= P10;

  comp1 : process ( P1 , P2 )
  begin
    if P2 = '1' then
      if falling_edge( P1 ) then
        case JK1 is
          when "00" => null;
          when "10" => QQB1 <= '1';
          when "01" => QQB1 <= '0';
          when "11" => QQB1 <= not QQB1;
          when others => null;
        end case;
      end if;
    else
      QQB1 <= '0';
    end if;
  end process comp1;
  compQ1 : process( QQB1 )
  begin
    if QQB1 = '1' then
      P12 <= '1';
      P13 <= '0';
    else
      P12 <= '0';
      P13 <= '1';
    end if;
  end process compQ1;
  comp2 : process ( P5 , P6 )
  begin
    if P6 = '1' then
      if falling_edge( P5 ) then
        case JK2 is
          when "00" => null;
          when "10" => QQB2 <= '1';
          when "01" => QQB2 <= '0';
          when "11" => QQB2 <= not QQB2;
          when others => null;
        end case;
      end if;
    else
      QQB2 <= '0';
    end if;
  end process comp2;
  compQ2 : process ( QQB2 )
  begin
    if QQB2 = '1' then
      P9 <= '1';
      P8 <= '0';
    else
      P9 <= '0';
      P8 <= '1';
    end if;
  end process compQ2;
end architecture rtl;

library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc74163 is
  generic (
    Initial_value : std_logic_vector( 3 downto 0 ) := "0110"
    );
  port (
    P1 : in  std_logic;
    P2 : in  std_logic;
    P3 : in  std_logic;
    P4 : in  std_logic;
    P5 : in  std_logic;
    P6 : in  std_logic;
    P7 : in  std_logic;
    P9 : in  std_logic;
    P10 : in  std_logic;
    P11 : out std_logic;
    P12 : out std_logic;
    P13 : out std_logic;
    P14 : out std_logic;
    P15 : out std_logic;
    P8 : inout std_logic;
    P16 : inout std_logic);
end entity hc74163;

architecture rtl of hc74163 is
  signal Q : std_logic_vector( 3 downto 0 ) := Initial_value;
  constant Qmax : std_logic_vector( Q'range ) := ( others => '1');
  signal P : std_logic_vector( 3 downto 0 );
begin
  -- assert P8 /= '0' report "A hc163 does not have his ground connected" severity failure;
  -- assert P16 /= '1' report "A hc163 does not have his vcc connected" severity failure;  
  P14 <= Q( 0 );
  P13 <= Q( 1 );
  P12 <= Q( 2 );
  P11 <= Q( 3 );
  P( 0 ) <= P3;
  P( 1 ) <= P4;
  P( 2 ) <= P5;
  P( 3 ) <= P6;
  P15 <= Q(0) and Q(1) and Q(2) and Q(3) and P10;
  main_proc : process ( P2 )
  begin
    if rising_edge(P2) then
      if P1 = '1' then
        if P9 = '1' then
          if P7 = '1' and P10 = '1' then
            if Q /= Qmax then
              Q <= std_logic_vector( (unsigned( Q ) + 1 ));
            else
              Q <= ( others => '0' );
            end if;
          end if;
        else
          Q <= P;
        end if;
      else
        Q <= ( others => '0' );
      end if;
    end if;
  end process main_proc;
end architecture rtl;

library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc74283 is
  port (
    P1 : out std_logic;
    P2 : in  std_logic;
    P3 : in  std_logic;
    P4 : out  std_logic;
    P5 : in  std_logic;
    P6 : in  std_logic;
    P7 : in  std_logic;
    P9 : out  std_logic;
    P10 : out  std_logic;
    P11 : in std_logic;
    P12 : in std_logic;
    P13 : out std_logic;
    P14 : in std_logic;
    P15 : in std_logic;
    P8 : inout std_logic;
    P16 : inout std_logic);
end entity hc74283;

architecture rtl of hc74283 is
  signal A : std_logic_vector( 5 downto 0 );
  signal B : std_logic_vector( 5 downto 0 );
  signal S : std_logic_vector( 5 downto 0 );
begin
  -- assert P8 /= '0' report "A hc283 does not have his ground connected" severity failure;
  -- assert P16 /= '1' report "A hc283 does not have his vcc connected" severity failure;  

  A( 0 ) <= P7;
  B( 0 ) <= P7;
  A( 1 ) <= P5;
  A( 2 ) <= P3;
  A( 3 ) <= P14;
  A( 4 ) <= P12;
  B( 1 ) <= P6;
  B( 2 ) <= P2;
  B( 3 ) <= P15;
  B( 4 ) <= P11;
  A( 5 ) <= '0';
  B( 5 ) <= '0';

  main_proc : process( A , B )
  begin
    S <= std_logic_vector( unsigned( A ) + unsigned( B ));
  end process main_proc;
  
  P4 <= S( 1 );
  P1 <= S( 2 );
  P13 <= S( 3 );
  P10 <= S( 4 );
  P9 <= S( 5 );
end architecture rtl;


library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc74273 is
  generic (
    Initial_value : std_logic_vector( 7 downto 0 ) := "00110100");
  port (
    P1 : in  std_logic;
    P2 : out  std_logic;
    P3 : in  std_logic;
    P4 : in  std_logic;
    P5 : out  std_logic;
    P6 : out  std_logic;
    P7 : in  std_logic;
    P8 : in  std_logic;
    P9 : out  std_logic;
    P11 : in std_logic;
    P12 : out std_logic;
    P13 : in std_logic;
    P14 : in std_logic;
    P15 : out std_logic;
    P16 : out std_logic;
    P17 : in std_logic;
    P18 : in std_logic;
    P19 : out std_logic;
    P10 : inout std_logic;
    P20 : inout std_logic);
end entity hc74273;

architecture rtl of hc74273 is
  signal D : std_logic_vector( 7 downto 0 );
  signal Q : std_logic_vector( D'range );
  signal Du : std_logic_vector( D'range ) := ( others => 'U' );
begin
  -- assert P10 /= '0' report "A hc273 does not have his ground connected" severity failure;
  -- assert P20 /= '1' report "A hc273 does not have his vcc connected" severity failure;
  D( 0 ) <= P3;
  D( 1 ) <= P4;
  D( 2 ) <= P7;
  D( 3 ) <= P8;
  D( 4 ) <= P13;
  D( 5 ) <= P14;
  D( 6 ) <= P17;
  D( 7 ) <= P18;
  main_proc : process ( P1 , P11 )
  begin
    if P1 = '1' then
      if rising_edge(P11) then
        if D /= Du then
          Q <= D;
        else
          Q <= Initial_value;
        end if;
      end if;
    else
      Q <= ( others => '0' );
    end if;
  end process main_proc;
  P2 <= Q( 0 );
  P5 <= Q( 1 );
  P6 <= Q( 2 );
  P9 <= Q( 3 );
  P12 <= Q( 4 );
  P15 <= Q( 5 );
  P16 <= Q( 6 );
  P19 <= Q( 7 );
end architecture rtl;


library IEEE;
use IEEE.Std_Logic_1164.all,
  ieee.numeric_std.all,
  work.cmos74hc.all;

-- Entity declaration

entity hc74138 is
  port (
    P1 : in  std_logic;
    P2 : in  std_logic;
    P3 : in  std_logic;
    P4 : in  std_logic;
    P5 : in  std_logic;
    P6 : in  std_logic;
    P7 : out  std_logic;
    P9 : out  std_logic;
    P10 : out  std_logic;
    P11 : out std_logic;
    P12 : out std_logic;
    P13 : out std_logic;
    P14 : out std_logic;
    P15 : out std_logic;
    P8 : inout std_logic;
    P16 : inout std_logic);
end entity hc74138;

architecture rtl of hc74138 is
begin
  -- assert P8 = '0' report "A hc138 does not have his ground connected" severity failure;
  -- assert P16 = '1' report "A hc138 does not have his vcc connected" severity failure;  
  main_proc : process ( P1, P2, P3, P4, P5, P6 )
    variable out_vect: std_logic_vector( 0 to 7 );
    variable in_vect : std_logic_vector( 2 downto 0 );
  begin
    if P4 = '0' and P5 = '0' and P6 = '1' then
      out_vect := (others => '1');
      in_vect( 0 ) := P1;
      in_vect( 1 ) := P2;
      in_vect( 2 ) := P3;
      case in_vect is
        when "000" => out_vect( 0 ) := '0';
        when "001" => out_vect( 1 ) := '0';
        when "010" => out_vect( 2 ) := '0';
        when "011" => out_vect( 3 ) := '0';
        when "100" => out_vect( 4 ) := '0';
        when "101" => out_vect( 5 ) := '0';
        when "110" => out_vect( 6 ) := '0';
        when "111" => out_vect( 7 ) := '0';
        when others => out_vect := (others => 'X');
      end case;
      P7 <= out_vect( 7 );
      P9 <= out_vect( 6 );
      P10 <= out_vect( 5 );
      P11 <= out_vect( 4 );
      P12 <= out_vect( 3 );
      P13 <= out_vect( 2 );
      P14 <= out_vect( 1 );
      P15 <= out_vect( 0 );
    else
      P7 <= '1';
      P9 <= '1';
      P10 <= '1';
      P11 <= '1';
      P12 <= '1';
      P13 <= '1';
      P14 <= '1';
      P15 <= '1';
    end if;
  end process main_proc;
end architecture rtl;

