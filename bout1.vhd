library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
-- array package
use work.block_types.all;


entity bout1 is 

   -- 10 pixel gaps in between blocks (use 5 pixels for leftmost and rightmost)
	-- each block is 25 pixels long, 10 pixels tall
  GENERIC(
    
	col_a_left : INTEGER := 0;
	col_a_right : INTEGER := 75;
	col_z_left : INTEGER := 565;
	col_z_right : INTEGER := 640;
	bottom_of_top : INTEGER := 75;
	
	row_one_top : INTEGER := 125;
	row_one_bottom : INTEGER := 130;
	
	row_two_top : INTEGER := 135;
	row_two_bottom : INTEGER := 140;
	
	paddle_top : INTEGER := 425;
	paddle_bottom : INTEGER := 430;
	
	block_left  : int_array := (
		 1 => 80,  2 => 115, 3 => 150, 4 => 185, 5 => 220,
		 6 => 255, 7 => 290, 8 => 325, 9 => 360, 10 => 395,
		 11 => 430, 12 => 465, 13 => 500, 14 => 535
	);
	block_right : int_array := (
		 1 => 105, 2 => 140, 3 => 175, 4 => 210, 5 => 245,
		 6 => 280, 7 => 315, 8 => 350, 9 => 385, 10 => 420,
		 11 => 455, 12 => 490, 13 => 525, 14 => 560
	)
	
	); 


	port ( 
		-- counter direction flip
		button_press : in std_logic; 
		
		-- internal clock
		max10_clk      :    IN STD_LOGIC; -- ALSO pixel clock for VGA mode being used

		hex5         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		hex4         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		hex3         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		hex2         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		hex1         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		hex0         : OUT STD_LOGIC_VECTOR(6 downto 0) := "1111111";
		
		
		-- set to CLK on rotary encoder as IO4
		Arduino_IO4  :    IN STD_LOGIC; --channel A for encoder
		-- set to DT on rotary encoder as IO5
		Arduino_IO5  :    IN STD_LOGIC; --channel B for encoder
		
		-- Inputs for image generation
		reset_n_m		:	IN	STD_LOGIC; --active low asycnchronous reset
		
		-- Outputs for image generation 
		
		h_sync_m		:	OUT	STD_LOGIC;	--horiztonal sync pulse
		v_sync_m		:	OUT	STD_LOGIC;	--vertical sync pulse 
		
		red_m      :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
		green_m    :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
		blue_m     :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); --blue magnitude output to DAC
		key0 :in std_logic
	);

end bout1;

ARCHITECTURE breakout_1p OF bout1 IS

	 -- 7-segment display component
    component bcd_7segment is 
        port(
            BCDin : in STD_LOGIC_VECTOR (3 downto 0);
            Seven_Segment : out STD_LOGIC_VECTOR (6 downto 0)
        );
    end component;

	component vga_pll_25_175 is 
	
		port(
		
			inclk0		:	IN  STD_LOGIC := '0';  -- Input clock that gets divided (50 MHz for max10)
			c0			:	OUT STD_LOGIC          -- Output clock for vga timing (25.175 MHz)
		
		);
		
	end component;
	
	component vga_controller is 
	
		port(
		
			pixel_clk	:	IN	STD_LOGIC;	--pixel clock at frequency of VGA mode being used
			reset_n		:	IN	STD_LOGIC;	--active low asycnchronous reset
			h_sync		:	OUT	STD_LOGIC;	--horiztonal sync pulse
			v_sync		:	OUT	STD_LOGIC;	--vertical sync pulse
			disp_ena		:	OUT	STD_LOGIC;	--display enable ('1' = display time, '0' = blanking time)
			column		:	OUT	INTEGER;	--horizontal pixel coordinate
			row			:	OUT	INTEGER;	--vertical pixel coordinate
			n_blank		:	OUT	STD_LOGIC;	--direct blacking output to DAC
			n_sync		:	OUT	STD_LOGIC   --sync-on-green output to DAC
		
		);
		
	end component;


	-- vga signals
	signal pll_OUT_to_vga_controller_IN, dispEn : STD_LOGIC;
	signal rowSignal, colSignal : INTEGER;


--U3	:	hw_image_generator port map(dispEn, rowSignal, colSignal, red_m, green_m, blue_m);

--moveable paddle signals (intialized to centered row values)
signal x_left : INTEGER := 295;
signal x_right : INTEGER := 345;

signal A_prev           : std_logic := '0';
signal B_prev           : std_logic := '0';
signal A_curr           : std_logic := '0';
signal B_curr           : std_logic := '0';

SIGNAL add_val : STD_LOGIC_VECTOR (1 downto 0) := "01";
signal count_clk : std_logic_vector (15 downto 0) := (others => '0');
signal encode_clk : std_logic := '0';


-- ball clock
signal ball_counter : INTEGER:= 125000;
signal ball_clk : std_logic := '0';

-- ball signals
--signal ball_top : INTEGER:= 235;
--signal ball_bottom : INTEGER:= 240;
signal ball_top : INTEGER:= 205;
signal ball_bottom : INTEGER:= 210;
signal ball_left : INTEGER := 150;
signal ball_right : INTEGER := 155;

-- intialized to left and down
	-- left = 0, right = 1
signal left_right : std_logic := '0'; 
	-- up = 0, down = 1
signal up_down : std_logic := '0'; 
signal reset_location : std_logic := '0'; 


-- block visibility signals
type block_on_array is array(1 to 28) of std_logic;
signal block_on : block_on_array := (1 to 28 => '1');

-- signals for displaying score to seven segment displays 2 downto 0
-- note: maximum score is 999
signal score : std_logic_vector (9 downto 0) := (others => '0');
signal hex_0_score     : std_logic_vector (3 downto 0) := "0000";
signal hex_1_score     : std_logic_vector (3 downto 0) := "0000";
signal hex_2_score    : std_logic_vector (3 downto 0) := "0000";
signal game_over_win : std_logic := '0';
signal game_over_loss : std_logic := '0';


-- signals for displaying number of lives to seven segment displays 5 downto 4
signal hex_5_lives    : std_logic_vector (3 downto 0) := "0000";
signal hex_4_lives    : std_logic_vector (3 downto 0) := "0011";

BEGIN

-- movement for the ball
ball_direction : process(pll_OUT_to_vga_controller_IN,key0)
begin

	if (key0 = '0') then  -- added aync reset fot ball_direction
		
		-- reset score
		hex_2_score <= "0000";
		hex_1_score <= "0000";
		hex_0_score <= "0000";
		
      reset_location <= '0'; --reset location to 0 becasause key0 does this, shouldnt intefere with the rest of the process
      block_on <= (others => '1'); --turn on all the blocks hitboxes 
		--wondering if i need to do something about the direction becasue it spawns in the top left
		-- on next ball "serve"
		-- go down and.....
		up_down <= '1';
		-- move left
		IF (max10_clk = '0') THEN
			left_right <= '0';
		-- move right
		ELSIF (max10_clk = '1') THEN
			left_right <= '1';
		ELSE
			left_right <= '0';
		END IF;
		
	elsif(rising_edge(pll_OUT_to_vga_controller_IN)) then 
	
		-- logic for score counter
		if((game_over_win = '1') or (game_over_loss = '1')) then
			-- pause game
		hex_5_lives <= "1111";	
		
		-- else, proceed as normal
		else
			if(hex_0_score >= "1001") then
				hex_0_score <= (others => '0');
				hex_1_score <= std_logic_vector(unsigned(hex_1_score) + unsigned(add_val));
			end if;
			
			if((hex_0_score >= "1001") and (hex_1_score >= "1001")) then
				hex_1_score <= (others => '0');
				hex_2_score <= std_logic_vector(unsigned(hex_2_score) + unsigned(add_val));	
			end if;
		
			if((hex_0_score >= "1000") and (hex_1_score >= "1001") and (hex_2_score >= "1001")) then 
				game_over_win <= '1';	-- stop game if this condition becomes 1
			end if;
		end if;
	
		 --move south-west by 1 pixel every 0.05 seconds
		 
		 -- if ball hits left border at 75 pixels, invert direction from left to right
		 if (ball_left <= col_a_right) then
			left_right <= '1';
		 -- if ball hits right border at 565 pixels, invert direction from right to left
		 elsif (ball_right >= col_z_left) then
			left_right <= '0';
		 -- if ball hits top border at 75 pixels, invert direction from up to down
		 elsif (ball_top <= 75) then
			up_down <= '1';
		 -- if ball hits bottom "pit" at 480 pixels, reset ball to initial position
		 -- AND does random direction on next ball placement
		 elsif (ball_bottom >= 480) then
			reset_location <= '1';
			
		 -- if hit the top of paddle and one of the ball's sides is between the limits of the paddle
		 elsif ( ball_bottom = paddle_top AND ((x_left <= ball_left AND ball_left <= x_right) OR (x_left <= ball_right AND ball_right <= x_right)) ) then
			-- rebound up
			up_down <= '0';
			
		 -- if hit the bottom of paddle and one of the ball's sides is between the limits of the paddle
		 elsif ( ball_top = paddle_bottom AND ((x_left <= ball_left AND ball_left <= x_right) OR (x_left <= ball_right AND ball_right <= x_right)) ) then
			-- rebound down
			up_down <= '1';
			
		 -- if hit the right side of paddle and either the top or bottom of the ball is between the limits of the paddle
		 elsif ( ball_left = x_right AND ((paddle_top <= ball_top AND ball_top <= paddle_bottom) OR (paddle_top <= ball_bottom AND ball_bottom <= paddle_bottom)) ) then
			-- rebound right
			left_right <= '1';
			
		 -- if hit the left side of paddle and either the top or bottom of the ball is between the limits of the paddle
		 elsif ( ball_right = x_left AND ((paddle_top <= ball_top AND ball_top <= paddle_bottom) OR (paddle_top <= ball_bottom AND ball_bottom <= paddle_bottom)) ) then
			-- rebound left
			left_right <= '0';
			
			-- BLOCK 1
			elsif ( block_on(1) = '1' AND ball_bottom = row_one_top AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
				-- rebound up
				up_down <= '0';
				block_on(1) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(1) = '1' AND ball_top = row_one_bottom AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
				-- rebound down
				up_down <= '1';
				block_on(1) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(1) = '1' AND ball_left = block_right(1) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(1) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(1) = '1' AND ball_right = block_left(1) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(1) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			
		-- BLOCK 2
			elsif ( block_on(2) = '1' AND ball_bottom = row_one_top AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
				-- rebound up
				up_down <= '0';
				block_on(2) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(2) = '1' AND ball_top = row_one_bottom AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
				-- rebound down
				up_down <= '1';
				block_on(2) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(2) = '1' AND ball_left = block_right(2) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(2) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(2) = '1' AND ball_right = block_left(2) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(2) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 3
			elsif ( block_on(3) = '1' AND ball_bottom = row_one_top AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
				-- rebound up
				up_down <= '0';
				block_on(3) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(3) = '1' AND ball_top = row_one_bottom AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
				-- rebound down
				up_down <= '1';
				block_on(3) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(3) = '1' AND ball_left = block_right(3) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(3) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(3) = '1' AND ball_right = block_left(3) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(3) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 4
			elsif ( block_on(4) = '1' AND ball_bottom = row_one_top AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
				-- rebound up
				up_down <= '0';
				block_on(4) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(4) = '1' AND ball_top = row_one_bottom AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
				-- rebound down
				up_down <= '1';
				block_on(4) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(4) = '1' AND ball_left = block_right(4) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(4) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(4) = '1' AND ball_right = block_left(4) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(4) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 5
			elsif ( block_on(5) = '1' AND ball_bottom = row_one_top AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
				-- rebound up
				up_down <= '0';
				block_on(5) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(5) = '1' AND ball_top = row_one_bottom AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
				-- rebound down
				up_down <= '1';
				block_on(5) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(5) = '1' AND ball_left = block_right(5) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(5) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(5) = '1' AND ball_right = block_left(5) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(5) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 6
			elsif ( block_on(6) = '1' AND ball_bottom = row_one_top AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
				-- rebound up
				up_down <= '0';
				block_on(6) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(6) = '1' AND ball_top = row_one_bottom AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
				-- rebound down
				up_down <= '1';
				block_on(6) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(6) = '1' AND ball_left = block_right(6) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(6) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(6) = '1' AND ball_right = block_left(6) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(6) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 7
			elsif ( block_on(7) = '1' AND ball_bottom = row_one_top AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
				-- rebound up
				up_down <= '0';
				block_on(7) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(7) = '1' AND ball_top = row_one_bottom AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
				-- rebound down
				up_down <= '1';
				block_on(7) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(7) = '1' AND ball_left = block_right(7) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(7) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(7) = '1' AND ball_right = block_left(7) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(7) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 8
			elsif ( block_on(8) = '1' AND ball_bottom = row_one_top AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
				-- rebound up
				up_down <= '0';
				block_on(8) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(8) = '1' AND ball_top = row_one_bottom AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
				-- rebound down
				up_down <= '1';
				block_on(8) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(8) = '1' AND ball_left = block_right(8) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(8) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(8) = '1' AND ball_right = block_left(8) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(8) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 9
			elsif ( block_on(9) = '1' AND ball_bottom = row_one_top AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
				-- rebound up
				up_down <= '0';
				block_on(9) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(9) = '1' AND ball_top = row_one_bottom AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
				-- rebound down
				up_down <= '1';
				block_on(9) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(9) = '1' AND ball_left = block_right(9) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(9) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(9) = '1' AND ball_right = block_left(9) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(9) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 10
			elsif ( block_on(10) = '1' AND ball_bottom = row_one_top AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
				-- rebound up
				up_down <= '0';
				block_on(10) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(10) = '1' AND ball_top = row_one_bottom AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
				-- rebound down
				up_down <= '1';
				block_on(10) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(10) = '1' AND ball_left = block_right(10) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(10) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(10) = '1'AND ball_right = block_left(10) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(10) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 11
			elsif ( block_on(11) = '1' AND ball_bottom = row_one_top AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
				-- rebound up
				up_down <= '0';
				block_on(11) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(11) = '1' AND ball_top = row_one_bottom AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
				-- rebound down
				up_down <= '1';
				block_on(11) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(11) = '1' AND ball_left = block_right(11) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(11) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(11) = '1' AND ball_right = block_left(11) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(11) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 12
			elsif ( block_on(12) = '1' AND ball_bottom = row_one_top AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
				-- rebound up
				up_down <= '0';
				block_on(12) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(12) = '1' AND ball_top = row_one_bottom AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
				-- rebound down
				up_down <= '1';
				block_on(12) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(12) = '1' AND ball_left = block_right(12) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(12) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(12) = '1' AND ball_right = block_left(12) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(12) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 13
			elsif ( block_on(13) = '1' AND ball_bottom = row_one_top AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
				-- rebound up
				up_down <= '0';
				block_on(13) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(13) = '1' AND ball_top = row_one_bottom AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
				-- rebound down
				up_down <= '1';
				block_on(13) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(13) = '1' AND ball_left = block_right(13) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(13) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(13) = '1' AND ball_right = block_left(13) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(13) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 28
			elsif ( block_on(14) = '1' AND ball_bottom = row_one_top AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
				-- rebound up
				up_down <= '0';
				block_on(14) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(14) = '1' AND ball_top = row_one_bottom AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
				-- rebound down
				up_down <= '1';
				block_on(14) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(14) = '1' AND ball_left = block_right(14) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(14) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(14) = '1' AND ball_right = block_left(14) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(14) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		
		-- BLOCK 15
			elsif ( block_on(15) = '1' AND ball_bottom = row_two_top AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
				-- rebound up
				up_down <= '0';
				block_on(15) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(15) = '1' AND ball_top = row_two_bottom AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
				-- rebound down
				up_down <= '1';
				block_on(15) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(15) = '1' AND ball_left = block_right(1) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(15) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(15) = '1' AND ball_right = block_left(1) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(15) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			
		-- BLOCK 16
			elsif ( block_on(16) = '1' AND ball_bottom = row_two_top AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
				-- rebound up
				up_down <= '0';
				block_on(16) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(16) = '1' AND ball_top = row_two_bottom AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
				-- rebound down
				up_down <= '1';
				block_on(16) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(16) = '1' AND ball_left = block_right(2) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(16) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(16) = '1' AND ball_right = block_left(2) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(16) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 17
			elsif ( block_on(17) = '1' AND ball_bottom = row_two_top AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
				-- rebound up
				up_down <= '0';
				block_on(17) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(17) = '1' AND ball_top = row_two_bottom AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
				-- rebound down
				up_down <= '1';
				block_on(17) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(17) = '1' AND ball_left = block_right(3) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(17) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(17) = '1' AND ball_right = block_left(3) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(17) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 18
			elsif ( block_on(18) = '1' AND ball_bottom = row_two_top AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
				-- rebound up
				up_down <= '0';
				block_on(18) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(18) = '1' AND ball_top = row_two_bottom AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
				-- rebound down
				up_down <= '1';
				block_on(18) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(18) = '1' AND ball_left = block_right(4) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(18) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(18) = '1' AND ball_right = block_left(4) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(18) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 19
			elsif ( block_on(19) = '1' AND ball_bottom = row_two_top AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
				-- rebound up
				up_down <= '0';
				block_on(19) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(19) = '1' AND ball_top = row_two_bottom AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
				-- rebound down
				up_down <= '1';
				block_on(19) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(19) = '1' AND ball_left = block_right(5) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(19) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(19) = '1' AND ball_right = block_left(5) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(19) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 20
			elsif ( block_on(20) = '1' AND ball_bottom = row_two_top AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
				-- rebound up
				up_down <= '0';
				block_on(20) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(20) = '1' AND ball_top = row_two_bottom AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
				-- rebound down
				up_down <= '1';
				block_on(20) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(20) = '1' AND ball_left = block_right(6) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(20) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(20) = '1' AND ball_right = block_left(6) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(20) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 21
			elsif ( block_on(21) = '1' AND ball_bottom = row_two_top AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
				-- rebound up
				up_down <= '0';
				block_on(21) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(21) = '1' AND ball_top = row_two_bottom AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
				-- rebound down
				up_down <= '1';
				block_on(21) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(21) = '1' AND ball_left = block_right(7) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(21) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(21) = '1' AND ball_right = block_left(7) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(21) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 22
			elsif ( block_on(22) = '1' AND ball_bottom = row_two_top AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
				-- rebound up
				up_down <= '0';
				block_on(22) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(22) = '1' AND ball_top = row_two_bottom AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
				-- rebound down
				up_down <= '1';
				block_on(22) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(22) = '1' AND ball_left = block_right(8) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(22) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(22) = '1' AND ball_right = block_left(8) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(22) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 23
			elsif ( block_on(23) = '1' AND ball_bottom = row_two_top AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
				-- rebound up
				up_down <= '0';
				block_on(23) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(23) = '1' AND ball_top = row_two_bottom AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
				-- rebound down
				up_down <= '1';
				block_on(23) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(23) = '1' AND ball_left = block_right(9) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(23) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(23) = '1' AND ball_right = block_left(9) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(23) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 24
			elsif ( block_on(24) = '1' AND ball_bottom = row_two_top AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
				-- rebound up
				up_down <= '0';
				block_on(24) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(24) = '1' AND ball_top = row_two_bottom AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
				-- rebound down
				up_down <= '1';
				block_on(24) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(24) = '1' AND ball_left = block_right(10) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(24) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(24) = '1'AND ball_right = block_left(10) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(24) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 25
			elsif ( block_on(25) = '1' AND ball_bottom = row_two_top AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
				-- rebound up
				up_down <= '0';
				block_on(25) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(25) = '1' AND ball_top = row_two_bottom AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
				-- rebound down
				up_down <= '1';
				block_on(25) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(25) = '1' AND ball_left = block_right(11) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(25) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(25) = '1' AND ball_right = block_left(11) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(25) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 26
			elsif ( block_on(26) = '1' AND ball_bottom = row_two_top AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
				-- rebound up
				up_down <= '0';
				block_on(26) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(26) = '1' AND ball_top = row_two_bottom AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
				-- rebound down
				up_down <= '1';
				block_on(26) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(26) = '1' AND ball_left = block_right(12) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(26) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(26) = '1' AND ball_right = block_left(12) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(26) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 27
			elsif ( block_on(27) = '1' AND ball_bottom = row_two_top AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
				-- rebound up
				up_down <= '0';
				block_on(27) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(27) = '1' AND ball_top = row_two_bottom AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
				-- rebound down
				up_down <= '1';
				block_on(27) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(27) = '1' AND ball_left = block_right(13) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(27) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(27) = '1' AND ball_right = block_left(13) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(27) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));
		-- BLOCK 28
			elsif ( block_on(28) = '1' AND ball_bottom = row_two_top AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
				-- rebound up
				up_down <= '0';
				block_on(28) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(28) = '1' AND ball_top = row_two_bottom AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
				-- rebound down
				up_down <= '1';
				block_on(28) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(28) = '1' AND ball_left = block_right(14) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound right
				left_right <= '1';
				block_on(28) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			elsif ( block_on(28) = '1' AND ball_right = block_left(14) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
				-- rebound left
				left_right <= '0';
				block_on(28) <= '0';
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));				
				
		 else
			reset_location <= '0';
		 end if;
	end if;
end process;

ball_movement : process(ball_clk,key0)
begin
if (key0 = '0') then  -- added async reset for ball movemnt
      
      ball_top    <= 155; --reset ball postion
      ball_bottom <= 160;
      ball_left   <= 100;
      ball_right  <= 105;
		
		-- reset lives
      hex_4_lives <= "0011";
		
		-- reset game over loss
		game_over_loss <= '0';
		--do i need to reset the ball dirrection?
      

else
IF(rising_edge(ball_clk)) THEN
	-- if lives have run out, game_over is set to 1
	if (hex_4_lives = "0000") then
		game_over_loss <= '1';
	end if;

	-- if ball has not fallen in the "pit"
	if (reset_location = '0') then
		-- moving left
		if(left_right = '0') then
			ball_left  <= ball_left - 1;
			ball_right <= ball_right - 1;
		-- moving right
		elsif (left_right = '1') then
			ball_left  <= ball_left + 1;
			ball_right <= ball_right +	1;
		end if;
		
		-- moving up
		if(up_down = '0') then
			ball_top <= ball_top - 1;
			ball_bottom <= ball_bottom - 1;
		-- moving down
		elsif (up_down = '1') then
			ball_top <= ball_top + 1;
			ball_bottom <= ball_bottom + 1;
		end if;
	-- reset to initial position, if ball has fallen in pit
	else
		ball_top <= 155;
		ball_bottom <= 160;
		ball_left <= 100;
		ball_right <= 105;
		if(game_over_loss /= '1') then
			hex_4_lives <= std_logic_vector(unsigned(hex_4_lives) - unsigned(add_val));
		end if;
	end if;
	end if;
END IF;
end process;	

-- 0.005 second clock for ball movement testing
ball_clock : process(max10_clk)
        begin
        if(rising_edge(max10_clk)) then 
            ball_counter <= ball_counter - 1;
            -- if count value has counted to 2,500,000, toggle ball_clock
                if (ball_counter <= 0) then
                    ball_clk <= not ball_clk;
                    ball_counter <= 125000;
                end if;
        end if;
end process;

-- 100 Hz clock for rotary encoder (low enough frequency to ignore noise)
prescale_clock : process(max10_clk)
        begin
        if(rising_edge(max10_clk)) then 
            count_clk <= std_logic_vector(unsigned(count_clk) + unsigned(add_val));

            -- if count value has counted to 500000, toggle prescaled_clock
                if (count_clk = "1100001101010000") then
                    encode_clk <= not encode_clk;
                    count_clk <= (others => '0');
                end if;
        end if;
end process;

rotary_encoder: process(encode_clk,key0)
		 begin
		 
		 
			if (key0 ='0') then --added async reset 
				x_left <= 295; --sets the paddle to its default location
				x_right <= 345;
				
				
				
			else	
			  if rising_edge(encode_clk) then
					-- Sample encoder inputs
					A_curr <= Arduino_IO4;
					B_curr <= Arduino_IO5;
					
					-- if signal A changes and is on the rising edge
					if (A_prev /= A_curr) then

					-- Determine direction
						 -- Clockwise --> right paddle movement
						 if A_curr = B_prev then
						 	  if (x_left <= col_a_right) then
								  x_left <= 75;
								  x_right <= 125;
							  else
								  x_left  <= x_left - 10;
								  x_right <= x_right - 10;
							  end if;
						 -- Counter-clockwise --> left paddle movement
						 else
							  if (x_right >= col_z_left) then
								  x_left <= 515;
								  x_right <= 565;
							  else
								  x_left  <= x_left + 10;
								  x_right <= x_right + 10;
							  end if;
						 end if;
					end if;

					-- Update previous values
					A_prev <= A_curr;
					B_prev <= B_curr;
			  end if;
			  end if;
 end process;

  -- display game elements
display: PROCESS(dispEn, rowSignal, colSignal)
  BEGIN

    IF(dispEn = '1') THEN        --display time
			--print left white bar border
			IF(col_a_left < colSignal AND colSignal < col_a_right) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			--print right white bar border
			ELSIF(col_z_left < colSignal AND colSignal < col_z_right) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
		  -- print white bar header
		  ELSIF(rowSignal < bottom_of_top) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			-- PADDLE
			ELSIF(x_left < colSignal AND colSignal < x_right AND paddle_top < rowSignal AND rowSignal < paddle_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			-- BALL
			ELSIF(ball_left < colSignal AND colSignal < ball_right AND ball_top < rowSignal AND rowSignal < ball_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			-- block 1
			ELSIF(block_on(1) = '1' AND block_left(1) < colSignal AND colSignal < block_right(1) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			-- block 2
			ELSIF(block_on(2) = '1' AND block_left(2) < colSignal AND colSignal < block_right(2) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 3
			ELSIF(block_on(3) = '1' AND block_left(3) < colSignal AND colSignal < block_right(3) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 4
			ELSIF(block_on(4) = '1' AND block_left(4) < colSignal AND colSignal < block_right(4) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 5
			ELSIF(block_on(5) = '1' AND block_left(5) < colSignal AND colSignal < block_right(5) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 6
			ELSIF(block_on(6) = '1' AND block_left(6) < colSignal AND colSignal < block_right(6) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 7
			ELSIF(block_on(7) = '1' AND block_left(7) < colSignal AND colSignal < block_right(7) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 8
			ELSIF(block_on(8) = '1' AND block_left(8) < colSignal AND colSignal < block_right(8) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 9
			ELSIF(block_on(9) = '1' AND block_left(9) < colSignal AND colSignal < block_right(9) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 10
			ELSIF(block_on(10) = '1' AND block_left(10) < colSignal AND colSignal < block_right(10) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 11
			ELSIF(block_on(11) = '1' AND block_left(11) < colSignal AND colSignal < block_right(11) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 12
			ELSIF(block_on(12) = '1' AND block_left(12) < colSignal AND colSignal < block_right(12) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 13
			ELSIF(block_on(13) = '1' AND block_left(13) < colSignal AND colSignal < block_right(13) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 14
			ELSIF(block_on(14) = '1' AND block_left(14) < colSignal AND colSignal < block_right(14) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			-- block 15
			ELSIF(block_on(15) = '1' AND block_left(1) < colSignal AND colSignal < block_right(1) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- block 16
			ELSIF(block_on(16) = '1' AND block_left(2) < colSignal AND colSignal < block_right(2) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 17
			ELSIF(block_on(17) = '1' AND block_left(3) < colSignal AND colSignal < block_right(3) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 18
			ELSIF(block_on(18) = '1' AND block_left(4) < colSignal AND colSignal < block_right(4) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 19
			ELSIF(block_on(19) = '1' AND block_left(5) < colSignal AND colSignal < block_right(5) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 20
			ELSIF(block_on(20) = '1' AND block_left(6) < colSignal AND colSignal < block_right(6) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 21
			ELSIF(block_on(21) = '1' AND block_left(7) < colSignal AND colSignal < block_right(7) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 22
			ELSIF(block_on(22) = '1' AND block_left(8) < colSignal AND colSignal < block_right(8) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 23
			ELSIF(block_on(23) = '1' AND block_left(9) < colSignal AND colSignal < block_right(9) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 24
			ELSIF(block_on(24) = '1' AND block_left(10) < colSignal AND colSignal < block_right(10) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 25
			ELSIF(block_on(25) = '1' AND block_left(11) < colSignal AND colSignal < block_right(11) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 26
			ELSIF(block_on(26) = '1' AND block_left(12) < colSignal AND colSignal < block_right(12) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 27
			ELSIF(block_on(27) = '1' AND block_left(13) < colSignal AND colSignal < block_right(13) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 28
			ELSIF(block_on(28) = '1' AND block_left(14) < colSignal AND colSignal < block_right(14) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
		  
		  
		  
		  -- rest of screen -> black
		  ELSE 
			red_m <= (OTHERS => '0');
			green_m <= (OTHERS => '0');
			blue_m <= (OTHERS => '0');
			END IF;
    ELSE                           --blanking time
      red_m <= (OTHERS => '0');
      green_m <= (OTHERS => '0');
      blue_m <= (OTHERS => '0');
    END IF;
  
  END PROCESS;
  
  



-- Just need 3 components for VGA system 
	U1	:	vga_pll_25_175 port map(max10_clk, pll_OUT_to_vga_controller_IN);
	U2	:	vga_controller port map(pll_OUT_to_vga_controller_IN, reset_n_m, h_sync_m, v_sync_m, dispEn, colSignal, rowSignal, open, open);
	--U3	:	hw_image_generator port map(dispEn, rowSignal, colSignal, red_m, green_m, blue_m);
  
  -- display score to seven segments 2 downto 0
    UScore0: bcd_7segment port map(hex_2_score, hex2);
    UScore1: bcd_7segment port map(hex_1_score, hex1);
    UScore2: bcd_7segment port map(hex_0_score, hex0);

    -- display lives to seven segements 5 downto 4
    ULives0: bcd_7segment port map(hex_5_lives, hex5);
    ULives1: bcd_7segment port map(hex_4_lives, hex4);
	 
end breakout_1p;
