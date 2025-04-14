library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package block_types is
    constant NUM_BLOCKS : integer := 14;
    type int_array is array(1 to NUM_BLOCKS) of integer;
end package;

package body block_types is
end package body;