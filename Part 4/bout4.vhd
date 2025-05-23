library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
-- array package
use work.block_types.all;


entity bout4 is 

   -- 10 pixel gaps in between blocks (use 5 pixels for leftmost and rightmost)
	-- each block is 25 pixels long, 10 pixels tall
  GENERIC(
    
	col_a_left : INTEGER := 0;
	col_a_right : INTEGER := 75;
	col_z_left : INTEGER := 565;
	col_z_right : INTEGER := 640;
	bottom_of_top : INTEGER := 75;
	
	row_one_top : INTEGER := 120;
	row_one_bottom : INTEGER := 155;
	
	row_two_top : INTEGER := 155;
	row_two_bottom : INTEGER := 190;
	
	paddle_top : INTEGER := 425;
	paddle_bottom : INTEGER := 430;
	
	-- segments for vga score/life display
	seg_t_top		: INTEGER := 15;
	seg_t_bottom	: INTEGER := 20;	
	seg_m_top		: INTEGER := 35;	
	seg_m_bottom	: INTEGER := 40;	
	seg_b_top		: INTEGER := 55;	
	seg_b_bottom	: INTEGER := 60;
	seg_l_left		: INTEGER := 110;
	seg_l_right		: INTEGER := 115;
	seg_r_left		: INTEGER := 125;
	seg_r_right		: INTEGER := 130;
	
	-- digit shift for VGA display
	-- p1_digit_2_score	: INTEGER := 0;	-- this is just to indicate that 0 is the baseline for all digits
	p1_digit_1_score	: INTEGER := 25;
	p1_digit_0_score	: INTEGER := 50;	
	
	p1_digit_1_lives	: INTEGER := 205;
	p1_digit_0_lives	: INTEGER := 230;
	
	p2_digit_2_score 	: INTEGER := 355;
	p2_digit_1_score 	: INTEGER := 380;
	p2_digit_0_score 	: INTEGER := 405;
	
	
	
	block_left  : int_array := (
		 1 => 75,  2 => 110, 3 => 145, 4 => 180, 5 => 215,
		 6 => 250, 7 => 285, 8 => 320, 9 => 355, 10 => 390,
		 11 => 425, 12 => 460, 13 => 495, 14 => 530
	);
	block_right : int_array := (
		 1 => 110, 2 => 145, 3 => 180, 4 => 215, 5 => 250,
		 6 => 285, 7 => 320, 8 => 355, 9 => 390, 10 => 425,
		 11 => 460, 12 => 495, 13 => 530, 14 => 565
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
		key1 : in std_logic;
		key0 : in std_logic;
		buzzer_out : out std_logic
	);

end bout4;

ARCHITECTURE breakout_1p OF bout4 IS

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
signal ball_top : INTEGER:= 230;
signal ball_bottom : INTEGER:= 237;
signal ball_left : INTEGER := 320;
signal ball_right : INTEGER := 327;

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
signal add_score : std_logic := '0';


-- signals for displaying number of lives to seven segment displays 5 downto 4
signal hex_5_lives    : std_logic_vector (3 downto 0) := "0000";
signal hex_4_lives    : std_logic_vector (3 downto 0) := "1001";



-- start/pause signal
signal start : std_logic := '0';
signal pll_intermediate : std_logic := '0';


-- english
signal english : std_logic := '0';
signal velocity_increase : std_logic := '0';

-- buzzer intermediate signal
signal buzzer : std_logic := '0';

BEGIN

start_pause : process (key1, key0)
	begin
		-- if the game is not over in SOME FORM
		if (game_over_win = '0' AND game_over_loss = '0') then
			-- if start/pause key is pressed
			if (rising_edge(key1)) then
				-- toggle from play to pause-or-pause-to-play
				start <= not start;
			end if;
		-- IF GAME IS OVER (if the game is either won or lost)
		else
			-- pause until reset
			start <= '0';
		end if;
end process;

-- movement for the ball
ball_direction : process(pll_intermediate,key0)
begin

	if (key0 = '0') then  -- added aync reset fot ball_direction
		
		-- reset score
		hex_2_score <= "0000";
		hex_1_score <= "0000";
		hex_0_score <= "0000";
		
      reset_location <= '0'; --reset location to 0 becasause key0 does this, shouldnt intefere with the rest of the process
      block_on <= (others => '1'); --turn on all the blocks hitboxes 
		game_over_win <= '0';
		
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
	elsif(rising_edge(pll_intermediate)) then 
	--elsif(rising_edge(pll_OUT_to_vga_controller_IN)) then 
	  -- if pause is not in effect
	  --IF (start = '1') THEN
		


			-- handle score conversion
			if(hex_0_score >= "1001") then
				
				if((hex_1_score >= "1001")) then				
				
					if((hex_2_score >= "1001")) then 
						-- game_over_win <= '1';	-- stop game if this condition becomes 1
					else
							hex_1_score <= (others => '0');
							hex_2_score <= std_logic_vector(unsigned(hex_2_score) + unsigned(add_val));
					end if;
				else
						hex_0_score <= (others => '0');
						hex_1_score <= std_logic_vector(unsigned(hex_1_score) + unsigned(add_val));
				end if;
			end if;
			
		
			-- adding score
			if (add_score = '1') then
				hex_0_score <= std_logic_vector(unsigned(hex_0_score) + unsigned(add_val));			
			end if;
		
			 --move south-west by 1 pixel every 0.05 seconds
			 
			 -- if ball hits left border at 75 pixels, invert direction from left to right
			 if (ball_left <= col_a_right) then
				left_right <= '1';
				add_score <= '0';
				buzzer <= '1';
			 -- if ball hits right border at 565 pixels, invert direction from right to left
			 elsif (ball_right >= col_z_left) then
				left_right <= '0';
				add_score <= '0';
				buzzer <= '1';
			 -- if ball hits top border at 75 pixels, invert direction from up to down
			 elsif (ball_top <= 75) then
				up_down <= '1';
				add_score <= '0';
				buzzer <= '1';
			 -- if ball hits bottom "pit" at 480 pixels, reset ball to initial position
			 -- AND does random direction on next ball placement
			 elsif (ball_bottom >= 480) then
				reset_location <= '1';
				add_score <= '0';
				buzzer <= '0';
				velocity_increase <= '0';
				
			 -- if hit the top of paddle and one of the ball's sides is between the limits of the paddle
			 elsif ( ball_bottom = paddle_top AND ((x_left <= ball_left AND ball_left <= x_right) OR (x_left <= ball_right AND ball_right <= x_right)) ) then
				-- rebound up
				if (english = '1') then
					up_down <= '0';
					left_right <= not left_right;
					velocity_increase <= '1';
				else
					up_down <= '0';
					velocity_increase <= '0';
				end if;
				add_score <= '0';
				buzzer <= '1';
				
			 -- if hit the bottom of paddle and one of the ball's sides is between the limits of the paddle
			 elsif ( ball_top = paddle_bottom AND ((x_left <= ball_left AND ball_left <= x_right) OR (x_left <= ball_right AND ball_right <= x_right)) ) then
				-- rebound down
				up_down <= '1';
				add_score <= '0';
				buzzer <= '1';
				
			 -- if hit the right side of paddle and either the top or bottom of the ball is between the limits of the paddle
			 elsif ( ball_left = x_right AND ((paddle_top <= ball_top AND ball_top <= paddle_bottom) OR (paddle_top <= ball_bottom AND ball_bottom <= paddle_bottom)) ) then
				-- rebound right
				left_right <= '1';
				add_score <= '0';
				buzzer <= '1';
				
			 -- if hit the left side of paddle and either the top or bottom of the ball is between the limits of the paddle
			 elsif ( ball_right = x_left AND ((paddle_top <= ball_top AND ball_top <= paddle_bottom) OR (paddle_top <= ball_bottom AND ball_bottom <= paddle_bottom)) ) then
				-- rebound left
				left_right <= '0';
				add_score <= '0';
				buzzer <= '1';
				
				-- BLOCK 1
				elsif ( block_on(1) = '1' AND ball_bottom = row_one_top AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
					-- rebound up
					up_down <= '0';
					block_on(1) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(1) = '1' AND ball_top = row_one_bottom AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
					-- rebound down
					up_down <= '1';
					block_on(1) <= '0';
					add_score <= '1';		
					buzzer <= '1';	
					elsif ( block_on(1) = '1' AND ball_left = block_right(1) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(1) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(1) = '1' AND ball_right = block_left(1) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(1) <= '0';
					add_score <= '1';
					buzzer <= '1';			
			-- BLOCK 2
				elsif ( block_on(2) = '1' AND ball_bottom = row_one_top AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
					-- rebound up
					up_down <= '0';
					block_on(2) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(2) = '1' AND ball_top = row_one_bottom AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
					-- rebound down
					up_down <= '1';
					block_on(2) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(2) = '1' AND ball_left = block_right(2) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(2) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(2) = '1' AND ball_right = block_left(2) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(2) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 3
				elsif ( block_on(3) = '1' AND ball_bottom = row_one_top AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
					-- rebound up
					up_down <= '0';
					block_on(3) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(3) = '1' AND ball_top = row_one_bottom AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
					-- rebound down
					up_down <= '1';
					block_on(3) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(3) = '1' AND ball_left = block_right(3) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(3) <= '0';
					add_score <= '1';		
					buzzer <= '1';
					elsif ( block_on(3) = '1' AND ball_right = block_left(3) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(3) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 4
				elsif ( block_on(4) = '1' AND ball_bottom = row_one_top AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
					-- rebound up
					up_down <= '0';
					block_on(4) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(4) = '1' AND ball_top = row_one_bottom AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
					-- rebound down
					up_down <= '1';
					block_on(4) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(4) = '1' AND ball_left = block_right(4) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(4) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(4) = '1' AND ball_right = block_left(4) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(4) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 5
				elsif ( block_on(5) = '1' AND ball_bottom = row_one_top AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
					-- rebound up
					up_down <= '0';
					block_on(5) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(5) = '1' AND ball_top = row_one_bottom AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
					-- rebound down
					up_down <= '1';
					block_on(5) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(5) = '1' AND ball_left = block_right(5) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(5) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(5) = '1' AND ball_right = block_left(5) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(5) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 6
				elsif ( block_on(6) = '1' AND ball_bottom = row_one_top AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
					-- rebound up
					up_down <= '0';
					block_on(6) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(6) = '1' AND ball_top = row_one_bottom AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
					-- rebound down
					up_down <= '1';
					block_on(6) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(6) = '1' AND ball_left = block_right(6) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(6) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(6) = '1' AND ball_right = block_left(6) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(6) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 7
				elsif ( block_on(7) = '1' AND ball_bottom = row_one_top AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
					-- rebound up
					up_down <= '0';
					block_on(7) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(7) = '1' AND ball_top = row_one_bottom AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
					-- rebound down
					up_down <= '1';
					block_on(7) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(7) = '1' AND ball_left = block_right(7) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(7) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(7) = '1' AND ball_right = block_left(7) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(7) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 8
				elsif ( block_on(8) = '1' AND ball_bottom = row_one_top AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
					-- rebound up
					up_down <= '0';
					block_on(8) <= '0';
					add_score <= '1';		
					buzzer <= '1';
					elsif ( block_on(8) = '1' AND ball_top = row_one_bottom AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
					-- rebound down
					up_down <= '1';
					block_on(8) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(8) = '1' AND ball_left = block_right(8) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(8) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(8) = '1' AND ball_right = block_left(8) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(8) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 9
				elsif ( block_on(9) = '1' AND ball_bottom = row_one_top AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
					-- rebound up
					up_down <= '0';
					block_on(9) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(9) = '1' AND ball_top = row_one_bottom AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
					-- rebound down
					up_down <= '1';
					block_on(9) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(9) = '1' AND ball_left = block_right(9) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(9) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(9) = '1' AND ball_right = block_left(9) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(9) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 10
				elsif ( block_on(10) = '1' AND ball_bottom = row_one_top AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
					-- rebound up
					up_down <= '0';
					block_on(10) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(10) = '1' AND ball_top = row_one_bottom AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
					-- rebound down
					up_down <= '1';
					block_on(10) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(10) = '1' AND ball_left = block_right(10) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(10) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(10) = '1'AND ball_right = block_left(10) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(10) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 11
				elsif ( block_on(11) = '1' AND ball_bottom = row_one_top AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
					-- rebound up
					up_down <= '0';
					block_on(11) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(11) = '1' AND ball_top = row_one_bottom AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
					-- rebound down
					up_down <= '1';
					block_on(11) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(11) = '1' AND ball_left = block_right(11) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(11) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(11) = '1' AND ball_right = block_left(11) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(11) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 12
				elsif ( block_on(12) = '1' AND ball_bottom = row_one_top AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
					-- rebound up
					up_down <= '0';
					block_on(12) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(12) = '1' AND ball_top = row_one_bottom AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
					-- rebound down
					up_down <= '1';
					block_on(12) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(12) = '1' AND ball_left = block_right(12) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(12) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(12) = '1' AND ball_right = block_left(12) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(12) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 13
				elsif ( block_on(13) = '1' AND ball_bottom = row_one_top AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
					-- rebound up
					up_down <= '0';
					block_on(13) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(13) = '1' AND ball_top = row_one_bottom AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
					-- rebound down
					up_down <= '1';
					block_on(13) <= '0';
					add_score <= '1';
					buzzer <= '1';	
					elsif ( block_on(13) = '1' AND ball_left = block_right(13) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(13) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(13) = '1' AND ball_right = block_left(13) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(13) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 14
				elsif ( block_on(14) = '1' AND ball_bottom = row_one_top AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
					-- rebound up
					up_down <= '0';
					block_on(14) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(14) = '1' AND ball_top = row_one_bottom AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
					-- rebound down
					up_down <= '1';
					block_on(14) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(14) = '1' AND ball_left = block_right(14) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(14) <= '0';
					add_score <= '1';
					buzzer <= '1';			
					elsif ( block_on(14) = '1' AND ball_right = block_left(14) AND ((row_one_top <= ball_top AND ball_top <= row_one_bottom) OR (row_one_top <= ball_bottom AND ball_bottom <= row_one_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(14) <= '0';
					add_score <= '1';
					buzzer <= '1';
			
			-- BLOCK 15
				elsif ( block_on(15) = '1' AND ball_bottom = row_two_top AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
					-- rebound up
					up_down <= '0';
					block_on(15) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(15) = '1' AND ball_top = row_two_bottom AND ((block_left(1) <= ball_left AND ball_left <= block_right(1)) OR (block_left(1) <= ball_right AND ball_right <= block_right(1))) ) then
					-- rebound down
					up_down <= '1';
					block_on(15) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(15) = '1' AND ball_left = block_right(1) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(15) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(15) = '1' AND ball_right = block_left(1) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(15) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
			-- BLOCK 16
				elsif ( block_on(16) = '1' AND ball_bottom = row_two_top AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
					-- rebound up
					up_down <= '0';
					block_on(16) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(16) = '1' AND ball_top = row_two_bottom AND ((block_left(2) <= ball_left AND ball_left <= block_right(2)) OR (block_left(2) <= ball_right AND ball_right <= block_right(2))) ) then
					-- rebound down
					up_down <= '1';
					block_on(16) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(16) = '1' AND ball_left = block_right(2) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(16) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(16) = '1' AND ball_right = block_left(2) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(16) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 17
				elsif ( block_on(17) = '1' AND ball_bottom = row_two_top AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
					-- rebound up
					up_down <= '0';
					block_on(17) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(17) = '1' AND ball_top = row_two_bottom AND ((block_left(3) <= ball_left AND ball_left <= block_right(3)) OR (block_left(3) <= ball_right AND ball_right <= block_right(3))) ) then
					-- rebound down
					up_down <= '1';
					block_on(17) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(17) = '1' AND ball_left = block_right(3) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(17) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(17) = '1' AND ball_right = block_left(3) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(17) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 18
				elsif ( block_on(18) = '1' AND ball_bottom = row_two_top AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
					-- rebound up
					up_down <= '0';
					block_on(18) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(18) = '1' AND ball_top = row_two_bottom AND ((block_left(4) <= ball_left AND ball_left <= block_right(4)) OR (block_left(4) <= ball_right AND ball_right <= block_right(4))) ) then
					-- rebound down
					up_down <= '1';
					block_on(18) <= '0';
					add_score <= '1';		
					buzzer <= '1';
					elsif ( block_on(18) = '1' AND ball_left = block_right(4) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(18) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(18) = '1' AND ball_right = block_left(4) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(18) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 19
				elsif ( block_on(19) = '1' AND ball_bottom = row_two_top AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
					-- rebound up
					up_down <= '0';
					block_on(19) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(19) = '1' AND ball_top = row_two_bottom AND ((block_left(5) <= ball_left AND ball_left <= block_right(5)) OR (block_left(5) <= ball_right AND ball_right <= block_right(5))) ) then
					-- rebound down
					up_down <= '1';
					block_on(19) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(19) = '1' AND ball_left = block_right(5) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(19) <= '0';
					add_score <= '1';		
					buzzer <= '1';
					elsif ( block_on(19) = '1' AND ball_right = block_left(5) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(19) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 20
				elsif ( block_on(20) = '1' AND ball_bottom = row_two_top AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
					-- rebound up
					up_down <= '0';
					block_on(20) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(20) = '1' AND ball_top = row_two_bottom AND ((block_left(6) <= ball_left AND ball_left <= block_right(6)) OR (block_left(6) <= ball_right AND ball_right <= block_right(6))) ) then
					-- rebound down
					up_down <= '1';
					block_on(20) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(20) = '1' AND ball_left = block_right(6) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(20) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(20) = '1' AND ball_right = block_left(6) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(20) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 21
				elsif ( block_on(21) = '1' AND ball_bottom = row_two_top AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
					-- rebound up
					up_down <= '0';
					block_on(21) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(21) = '1' AND ball_top = row_two_bottom AND ((block_left(7) <= ball_left AND ball_left <= block_right(7)) OR (block_left(7) <= ball_right AND ball_right <= block_right(7))) ) then
					-- rebound down
					up_down <= '1';
					block_on(21) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(21) = '1' AND ball_left = block_right(7) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(21) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(21) = '1' AND ball_right = block_left(7) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(21) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 22
				elsif ( block_on(22) = '1' AND ball_bottom = row_two_top AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
					-- rebound up
					up_down <= '0';
					block_on(22) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(22) = '1' AND ball_top = row_two_bottom AND ((block_left(8) <= ball_left AND ball_left <= block_right(8)) OR (block_left(8) <= ball_right AND ball_right <= block_right(8))) ) then
					-- rebound down
					up_down <= '1';
					block_on(22) <= '0';
					add_score <= '1';	
			      buzzer <= '1';		
					elsif ( block_on(22) = '1' AND ball_left = block_right(8) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(22) <= '0';
					add_score <= '1';	
			      buzzer <= '1';		
					elsif ( block_on(22) = '1' AND ball_right = block_left(8) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(22) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 23
				elsif ( block_on(23) = '1' AND ball_bottom = row_two_top AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
					-- rebound up
					up_down <= '0';
					block_on(23) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(23) = '1' AND ball_top = row_two_bottom AND ((block_left(9) <= ball_left AND ball_left <= block_right(9)) OR (block_left(9) <= ball_right AND ball_right <= block_right(9))) ) then
					-- rebound down
					up_down <= '1';
					block_on(23) <= '0';
					add_score <= '1';	
			      buzzer <= '1';		
					elsif ( block_on(23) = '1' AND ball_left = block_right(9) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(23) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(23) = '1' AND ball_right = block_left(9) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(23) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 24
				elsif ( block_on(24) = '1' AND ball_bottom = row_two_top AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
					-- rebound up
					up_down <= '0';
					block_on(24) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(24) = '1' AND ball_top = row_two_bottom AND ((block_left(10) <= ball_left AND ball_left <= block_right(10)) OR (block_left(10) <= ball_right AND ball_right <= block_right(10))) ) then
					-- rebound down
					up_down <= '1';
					block_on(24) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(24) = '1' AND ball_left = block_right(10) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(24) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(24) = '1'AND ball_right = block_left(10) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(24) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 25
				elsif ( block_on(25) = '1' AND ball_bottom = row_two_top AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
					-- rebound up
					up_down <= '0';
					block_on(25) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(25) = '1' AND ball_top = row_two_bottom AND ((block_left(11) <= ball_left AND ball_left <= block_right(11)) OR (block_left(11) <= ball_right AND ball_right <= block_right(11))) ) then
					-- rebound down
					up_down <= '1';
					block_on(25) <= '0';
					add_score <= '1';	
			      buzzer <= '1';		
					elsif ( block_on(25) = '1' AND ball_left = block_right(11) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(25) <= '0';
					add_score <= '1';	
			      buzzer <= '1';		
					elsif ( block_on(25) = '1' AND ball_right = block_left(11) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(25) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 26
				elsif ( block_on(26) = '1' AND ball_bottom = row_two_top AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
					-- rebound up
					up_down <= '0';
					block_on(26) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(26) = '1' AND ball_top = row_two_bottom AND ((block_left(12) <= ball_left AND ball_left <= block_right(12)) OR (block_left(12) <= ball_right AND ball_right <= block_right(12))) ) then
					-- rebound down
					up_down <= '1';
					block_on(26) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(26) = '1' AND ball_left = block_right(12) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(26) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					elsif ( block_on(26) = '1' AND ball_right = block_left(12) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(26) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 27
				elsif ( block_on(27) = '1' AND ball_bottom = row_two_top AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
					-- rebound up
					up_down <= '0';
					block_on(27) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(27) = '1' AND ball_top = row_two_bottom AND ((block_left(13) <= ball_left AND ball_left <= block_right(13)) OR (block_left(13) <= ball_right AND ball_right <= block_right(13))) ) then
					-- rebound down
					up_down <= '1';
					block_on(27) <= '0';
					add_score <= '1';
					buzzer <= '1';
					elsif ( block_on(27) = '1' AND ball_left = block_right(13) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right
					left_right <= '1';
					block_on(27) <= '0';
					add_score <= '1';	
					buzzer <= '1';		
					elsif ( block_on(27) = '1' AND ball_right = block_left(13) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left
					left_right <= '0';
					block_on(27) <= '0';
					add_score <= '1';
					buzzer <= '1';
			-- BLOCK 28
				elsif ( block_on(28) = '1' AND ball_bottom = row_two_top AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
					-- rebound up	
					up_down <= '0';
					block_on(28) <= '0';
					add_score <= '1';
					buzzer <= '1';		
					elsif ( block_on(28) = '1' AND ball_top = row_two_bottom AND ((block_left(14) <= ball_left AND ball_left <= block_right(14)) OR (block_left(14) <= ball_right AND ball_right <= block_right(14))) ) then
					-- rebound down					
					up_down <= '1';
					block_on(28) <= '0';
					add_score <= '1';	
				   buzzer <= '1';		
					elsif ( block_on(28) = '1' AND ball_left = block_right(14) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound right				
					left_right <= '1';
					block_on(28) <= '0';
					add_score <= '1';
				   buzzer <= '1';	
					
					elsif ( block_on(28) = '1' AND ball_right = block_left(14) AND ((row_two_top <= ball_top AND ball_top <= row_two_bottom) OR (row_two_top <= ball_bottom AND ball_bottom <= row_two_bottom)) ) then
					-- rebound left								
					left_right <= '0';
					block_on(28) <= '0';
					add_score <= '1';	
					buzzer <= '1';
					
					-- if all blocks are off, set game_over_win = '1'
					ELSIF((block_on(1) = '0') and (block_on(2) = '0') and (block_on(3) = '0') and (block_on(4) = '0') and (block_on(5) = '0') and (block_on(6) = '0') and (block_on(7) = '0') and (block_on(8) = '0') and (block_on(9) = '0') and (block_on(10) = '0') and (block_on(11) = '0') and (block_on(12) = '0') and (block_on(13) = '0') and (block_on(14) = '0') and (block_on(15) = '0') and (block_on(16) = '0') and (block_on(17) = '0') and (block_on(18) = '0') and (block_on(19) = '0') and (block_on(20) = '0') and (block_on(21) = '0') and (block_on(22) = '0') and (block_on(23) = '0') and (block_on(24) = '0') and (block_on(25) = '0') and (block_on(26) = '0') and (block_on(27) = '0') and (block_on(28) = '0')) then
						game_over_win <= '1';
						buzzer <= '0';
			 else
				reset_location <= '0';
				add_score <= '0';
				buzzer <= '0';
				
			 end if;
		--END IF;
	end if;
end process;


-- sends noise to buzzer
noise : process(buzzer)
begin
		buzzer_out <= buzzer;
end process;

ball_movement : process(ball_clk,key0)
begin
if (key0 = '0') then  -- added async reset for ball movemnt
      
      ball_top    <= 230; --reset ball postion
      ball_bottom <= 237;
      ball_left   <= 320;
      ball_right  <= 327;
		
		-- reset lives
      hex_4_lives <= "1001";
		
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
			-- "English" check to determine velocity at paddle collision
			--if(velocity_increase = '0') then
				ball_left  <= ball_left - 1;
				ball_right <= ball_right - 1;
			--else
			   --ball_left  <= ball_left - 2;
				--ball_right <= ball_right - 2;
			--end if;
		-- moving right
		elsif (left_right = '1') then
			-- "English" check to determine velocity at paddle collision
			--if(velocity_increase = '0') then
				ball_left  <= ball_left + 1;
				ball_right <= ball_right +	1;
			--else
			   --ball_left  <= ball_left + 2;
				--ball_right <= ball_right + 2;
			--end if;
		end if;
		
		-- moving up
		if(up_down = '0') then
			-- "English" check to determine velocity at paddle collision
			--if(velocity_increase = '0') then
				ball_top <= ball_top - 1;
				ball_bottom <= ball_bottom - 1;
			--else
				--ball_top <= ball_top - 2;
				--ball_bottom <= ball_bottom - 2;
			--end if;
		-- moving down
		elsif (up_down = '1') then
			-- "English" check to determine velocity at paddle collision
			--if(velocity_increase = '0') then
				ball_top <= ball_top + 1;
				ball_bottom <= ball_bottom + 1;
			--else
				--ball_top <= ball_top + 2;
				--ball_bottom <= ball_bottom + 2;
			--end if;
		end if;
	-- reset to initial position, if ball has fallen in pit
	else
      ball_top    <= 230;
      ball_bottom <= 237;
      ball_left   <= 320;
      ball_right  <= 327;
		if(game_over_loss /= '1') then
			hex_4_lives <= std_logic_vector(unsigned(hex_4_lives) - unsigned(add_val));
		end if;
	end if;
	end if;
END IF;
end process;	

pll_check : process(pll_OUT_to_vga_controller_IN, start)
begin
	if(start = '1') then
		pll_intermediate <= pll_OUT_to_vga_controller_IN;
	end if;
end process;

-- 0.05 second clock for ball movement testing
ball_clock : process(max10_clk, start)
        begin
		  -- if pause is not in effect
        IF (start = '1') THEN
			  if(rising_edge(max10_clk)) then 
					ball_counter <= ball_counter - 1;
					-- if count value has counted to 250,000, toggle ball_clock
						 if (ball_counter <= 0) then
							  ball_clk <= not ball_clk;
							  if (velocity_increase = '0') then
									ball_counter <= 125000;
							  else
									ball_counter <= 100000;
							  end if;
						 end if;
			  end if;
		  END IF;
end process;

-- 100 Hz clock for rotary encoder (low enough frequency to ignore noise)
prescale_clock : process(max10_clk, start)
        begin
		  -- if pause is not in effect
        IF (start = '1') THEN
			  if(rising_edge(max10_clk)) then 
					count_clk <= std_logic_vector(unsigned(count_clk) + unsigned(add_val));

					-- if count value has counted to 500000, toggle prescaled_clock
						 if (count_clk = "1100001101010000") then
							  encode_clk <= not encode_clk;
							  count_clk <= (others => '0');
						 end if;
			  end if;
		  END IF;
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
									-- if paddle moving left as ball is moving right
									if (left_right = '1') then
										english <= '1';
									else
										english <= '0';
									end if;
							  end if;
						 -- Counter-clockwise --> left paddle movement
						 else
							  if (x_right >= col_z_left) then
								  x_left <= 515;
								  x_right <= 565;
							  else
								  x_left  <= x_left + 10;
								  x_right <= x_right + 10;
									-- if paddle moving right as ball is moving left
									if (left_right = '0') then
										english <= '1';
									else
										english <= '0';
									end if;
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
			--print left border
			IF(col_a_left < colSignal AND colSignal < col_a_right AND rowSignal >= bottom_of_top) THEN
			  red_m <= "00001001";
			  green_m  <= (OTHERS => '0');
			  blue_m <= "00001111";
			--print right border
			ELSIF(col_z_left < colSignal AND colSignal < col_z_right AND rowSignal >= bottom_of_top) THEN
			  red_m <= "00001001";
			  green_m  <= (OTHERS => '0');
			  blue_m <= "00001111";
			-- print header
			ELSIF(rowSignal > 70 AND rowSignal < bottom_of_top) THEN
			  red_m <= "00001001";
			  green_m  <= (OTHERS => '0');
			  blue_m <= "00001111";
			  
			-- print score digit 2
			-- zero
			-- print left segment of zero
			ELSIF(hex_2_score = "0000" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of zero;
			ELSIF(hex_2_score = "0000" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of zero
			ELSIF(hex_2_score = "0000" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of zero
			ELSIF(hex_2_score = "0000" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- one
			-- print right segment of one
			ELSIF(hex_2_score = "0001" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- two
			-- print top segment of two
			ELSIF(hex_2_score = "0010" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of two
			ELSIF(hex_2_score = "0010" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of two
			ELSIF(hex_2_score = "0010" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of two
			ELSIF(hex_2_score = "0010" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of two
			ELSIF(hex_2_score = "0010" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- three
			-- print top segment of three
			ELSIF(hex_2_score = "0011" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of three
			ELSIF(hex_2_score = "0011" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print bottom segment of three
			ELSIF(hex_2_score = "0011" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of three
			ELSIF(hex_2_score = "0011" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- four
			-- print left segment of four
			ELSIF(hex_2_score = "0100" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');			
			-- print middle segment of four
			ELSIF(hex_2_score = "0100" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of four
			ELSIF(hex_2_score = "0100" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- five
			-- print top segment of five
			ELSIF(hex_2_score = "0101" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of five
			ELSIF(hex_2_score = "0101" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of five
			ELSIF(hex_2_score = "0101" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of five
			ELSIF(hex_2_score = "0101" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of five
			ELSIF(hex_2_score = "0101" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			
			
			-- six
			-- print top segment of six
			ELSIF(hex_2_score = "0110" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of six
			ELSIF(hex_2_score = "0110" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of six
			ELSIF(hex_2_score = "0110" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of six
			ELSIF(hex_2_score = "0110" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of six
			ELSIF(hex_2_score = "0110" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- seven
			-- print top segment of seven
			ELSIF(hex_2_score = "0111" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of seven
			ELSIF(hex_2_score = "0111" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- eight
			-- print left segment of eight
			ELSIF(hex_2_score = "1000" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of eight;
			ELSIF(hex_2_score = "1000" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of eight
			ELSIF(hex_2_score = "1000" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of eight
			ELSIF(hex_2_score = "1000" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of eight
			ELSIF(hex_2_score = "1000" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- nine
			-- print top segment of nine
			ELSIF(hex_2_score = "1001" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of nine;
			ELSIF(hex_2_score = "1001" and seg_r_left <= colSignal AND colSignal <= seg_r_right AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of nine
			ELSIF(hex_2_score = "1001" and seg_l_left <= colSignal AND colSignal <= seg_r_right AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of nine
			ELSIF(hex_2_score = "1001" and seg_l_left <= colSignal AND colSignal <= seg_l_right AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			
			-- print score digit 1
			-- zero
			-- print left segment of zero
			ELSIF(hex_1_score = "0000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of zero;
			ELSIF(hex_1_score = "0000" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of zero
			ELSIF(hex_1_score = "0000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of zero
			ELSIF(hex_1_score = "0000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- one
			-- print right segment of one
			ELSIF(hex_1_score = "0001" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- two
			-- print top segment of two
			ELSIF(hex_1_score = "0010" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of two
			ELSIF(hex_1_score = "0010" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of two
			ELSIF(hex_1_score = "0010" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of two
			ELSIF(hex_1_score = "0010" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of two
			ELSIF(hex_1_score = "0010" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- three
			-- print top segment of three
			ELSIF(hex_1_score = "0011" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of three
			ELSIF(hex_1_score = "0011" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print bottom segment of three
			ELSIF(hex_1_score = "0011" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of three
			ELSIF(hex_1_score = "0011" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- four
			-- print left segment of four
			ELSIF(hex_1_score = "0100" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');			
			-- print middle segment of four
			ELSIF(hex_1_score = "0100" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of four
			ELSIF(hex_1_score = "0100" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- five
			-- print top segment of five
			ELSIF(hex_1_score = "0101" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of five
			ELSIF(hex_1_score = "0101" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of five
			ELSIF(hex_1_score = "0101" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of five
			ELSIF(hex_1_score = "0101" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of five
			ELSIF(hex_1_score = "0101" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			
			
			-- six
			-- print top segment of six
			ELSIF(hex_1_score = "0110" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of six
			ELSIF(hex_1_score = "0110" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of six
			ELSIF(hex_1_score = "0110" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of six
			ELSIF(hex_1_score = "0110" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of six
			ELSIF(hex_1_score = "0110" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- seven
			-- print top segment of seven
			ELSIF(hex_1_score = "0111" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of seven
			ELSIF(hex_1_score = "0111" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- eight
			-- print left segment of eight
			ELSIF(hex_1_score = "1000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of eight;
			ELSIF(hex_1_score = "1000" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of eight
			ELSIF(hex_1_score = "1000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of eight
			ELSIF(hex_1_score = "1000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of eight
			ELSIF(hex_1_score = "1000" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- nine
			-- print top segment of nine
			ELSIF(hex_1_score = "1001" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of nine;
			ELSIF(hex_1_score = "1001" and seg_r_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of nine
			ELSIF(hex_1_score = "1001" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_r_right + p1_digit_1_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of nine
			ELSIF(hex_1_score = "1001" and seg_l_left + p1_digit_1_score <= colSignal AND colSignal <= seg_l_right + p1_digit_1_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			
			-- print score digit 0
			-- zero
			-- print left segment of zero
			ELSIF(hex_0_score = "0000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of zero;
			ELSIF(hex_0_score = "0000" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of zero
			ELSIF(hex_0_score = "0000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of zero
			ELSIF(hex_0_score = "0000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- one
			-- print right segment of one
			ELSIF(hex_0_score = "0001" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- two
			-- print top segment of two
			ELSIF(hex_0_score = "0010" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of two
			ELSIF(hex_0_score = "0010" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of two
			ELSIF(hex_0_score = "0010" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of two
			ELSIF(hex_0_score = "0010" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of two
			ELSIF(hex_0_score = "0010" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- three
			-- print top segment of three
			ELSIF(hex_0_score = "0011" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of three
			ELSIF(hex_0_score = "0011" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print bottom segment of three
			ELSIF(hex_0_score = "0011" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of three
			ELSIF(hex_0_score = "0011" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- four
			-- print left segment of four
			ELSIF(hex_0_score = "0100" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');			
			-- print middle segment of four
			ELSIF(hex_0_score = "0100" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of four
			ELSIF(hex_0_score = "0100" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- five
			-- print top segment of five
			ELSIF(hex_0_score = "0101" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of five
			ELSIF(hex_0_score = "0101" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of five
			ELSIF(hex_0_score = "0101" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of five
			ELSIF(hex_0_score = "0101" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of five
			ELSIF(hex_0_score = "0101" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			
			
			-- six
			-- print top segment of six
			ELSIF(hex_0_score = "0110" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of six
			ELSIF(hex_0_score = "0110" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of six
			ELSIF(hex_0_score = "0110" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of six
			ELSIF(hex_0_score = "0110" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of six
			ELSIF(hex_0_score = "0110" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- seven
			-- print top segment of seven
			ELSIF(hex_0_score = "0111" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of seven
			ELSIF(hex_0_score = "0111" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- eight
			-- print left segment of eight
			ELSIF(hex_0_score = "1000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of eight;
			ELSIF(hex_0_score = "1000" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of eight
			ELSIF(hex_0_score = "1000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of eight
			ELSIF(hex_0_score = "1000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of eight
			ELSIF(hex_0_score = "1000" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- nine
			-- print top segment of nine
			ELSIF(hex_0_score = "1001" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of nine;
			ELSIF(hex_0_score = "1001" and seg_r_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of nine
			ELSIF(hex_0_score = "1001" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_r_right + p1_digit_0_score AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of nine
			ELSIF(hex_0_score = "1001" and seg_l_left + p1_digit_0_score <= colSignal AND colSignal <= seg_l_right + p1_digit_0_score AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			
			-- print lives digit 1
			-- zero
			-- print left segment of zero
			ELSIF(hex_5_lives = "0000" and seg_l_left + p1_digit_1_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_1_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of zero;
			ELSIF(hex_5_lives = "0000" and seg_r_left + p1_digit_1_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_1_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of zero
			ELSIF(hex_5_lives = "0000" and seg_l_left + p1_digit_1_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_1_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of zero
			ELSIF(hex_5_lives = "0000" and seg_l_left + p1_digit_1_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_1_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- print lives digit 0
			-- zero
			-- print left segment of zero
			ELSIF(hex_4_lives = "0000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of zero;
			ELSIF(hex_4_lives = "0000" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of zero
			ELSIF(hex_4_lives = "0000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of zero
			ELSIF(hex_4_lives = "0000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- one
			-- print right segment of one
			ELSIF(hex_4_lives = "0001" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- two
			-- print top segment of two
			ELSIF(hex_4_lives = "0010" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of two
			ELSIF(hex_4_lives = "0010" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of two
			ELSIF(hex_4_lives = "0010" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of two
			ELSIF(hex_4_lives = "0010" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of two
			ELSIF(hex_4_lives = "0010" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- three
			-- print top segment of three
			ELSIF(hex_4_lives = "0011" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of three
			ELSIF(hex_4_lives = "0011" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print bottom segment of three
			ELSIF(hex_4_lives = "0011" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of three
			ELSIF(hex_4_lives = "0011" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- four
			-- print left segment of four
			ELSIF(hex_4_lives = "0100" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');			
			-- print middle segment of four
			ELSIF(hex_4_lives = "0100" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			-- print right segment of four
			ELSIF(hex_4_lives = "0100" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- five
			-- print top segment of five
			ELSIF(hex_4_lives = "0101" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of five
			ELSIF(hex_4_lives = "0101" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of five
			ELSIF(hex_4_lives = "0101" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of five
			ELSIF(hex_4_lives = "0101" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of five
			ELSIF(hex_4_lives = "0101" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			
			
			-- six
			-- print top segment of six
			ELSIF(hex_4_lives = "0110" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of six
			ELSIF(hex_4_lives = "0110" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of six
			ELSIF(hex_4_lives = "0110" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of six
			ELSIF(hex_4_lives = "0110" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of six
			ELSIF(hex_4_lives = "0110" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_m_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');  
			  
			  
			-- seven
			-- print top segment of seven
			ELSIF(hex_4_lives = "0111" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of seven
			ELSIF(hex_4_lives = "0111" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- eight
			-- print left segment of eight
			ELSIF(hex_4_lives = "1000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of eight;
			ELSIF(hex_4_lives = "1000" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print top segment of eight
			ELSIF(hex_4_lives = "1000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of eight
			ELSIF(hex_4_lives = "1000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print bottom segment of eight
			ELSIF(hex_4_lives = "1000" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_b_bottom and seg_b_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			  
			-- nine
			-- print top segment of nine
			ELSIF(hex_4_lives = "1001" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_t_bottom and seg_t_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print right segment of nine;
			ELSIF(hex_4_lives = "1001" and seg_r_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_b_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print middle segment of nine
			ELSIF(hex_4_lives = "1001" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_r_right + p1_digit_0_lives AND rowSignal <= seg_m_bottom and seg_m_top <= rowSignal) then
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- print left segment of nine
			ELSIF(hex_4_lives = "1001" and seg_l_left + p1_digit_0_lives <= colSignal AND colSignal <= seg_l_right + p1_digit_0_lives AND rowSignal >= seg_t_top AND seg_m_bottom >= rowSignal) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			
			
			-- PADDLE
			ELSIF(x_left < colSignal AND colSignal < x_right AND paddle_top < rowSignal AND rowSignal < paddle_bottom) THEN
			  red_m <= "00001111";
			  green_m  <= "00001111";
			  blue_m <= (OTHERS => '0');
			  
			-- BALL
			ELSIF(ball_left < colSignal AND colSignal < ball_right AND ball_top < rowSignal AND rowSignal < ball_bottom) THEN
			  red_m <= "00001111";
			  green_m  <= "00001111";
			  blue_m <= (OTHERS => '0');
			  
			-- block 1
			ELSIF(block_on(1) = '1' AND block_left(1) < colSignal AND colSignal < block_right(1) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			-- block 2
			ELSIF(block_on(2) = '1' AND block_left(2) < colSignal AND colSignal < block_right(2) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 3
			ELSIF(block_on(3) = '1' AND block_left(3) < colSignal AND colSignal < block_right(3) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 4
			ELSIF(block_on(4) = '1' AND block_left(4) < colSignal AND colSignal < block_right(4) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 5
			ELSIF(block_on(5) = '1' AND block_left(5) < colSignal AND colSignal < block_right(5) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 6
			ELSIF(block_on(6) = '1' AND block_left(6) < colSignal AND colSignal < block_right(6) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 7
			ELSIF(block_on(7) = '1' AND block_left(7) < colSignal AND colSignal < block_right(7) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 8
			ELSIF(block_on(8) = '1' AND block_left(8) < colSignal AND colSignal < block_right(8) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 9
			ELSIF(block_on(9) = '1' AND block_left(9) < colSignal AND colSignal < block_right(9) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 10
			ELSIF(block_on(10) = '1' AND block_left(10) < colSignal AND colSignal < block_right(10) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 11
			ELSIF(block_on(11) = '1' AND block_left(11) < colSignal AND colSignal < block_right(11) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 12
			ELSIF(block_on(12) = '1' AND block_left(12) < colSignal AND colSignal < block_right(12) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 13
			ELSIF(block_on(13) = '1' AND block_left(13) < colSignal AND colSignal < block_right(13) AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
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
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 17
			ELSIF(block_on(17) = '1' AND block_left(3) < colSignal AND colSignal < block_right(3) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 18
			ELSIF(block_on(18) = '1' AND block_left(4) < colSignal AND colSignal < block_right(4) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 19
			ELSIF(block_on(19) = '1' AND block_left(5) < colSignal AND colSignal < block_right(5) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 20
			ELSIF(block_on(20) = '1' AND block_left(6) < colSignal AND colSignal < block_right(6) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 21
			ELSIF(block_on(21) = '1' AND block_left(7) < colSignal AND colSignal < block_right(7) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 22
			ELSIF(block_on(22) = '1' AND block_left(8) < colSignal AND colSignal < block_right(8) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 23
			ELSIF(block_on(23) = '1' AND block_left(9) < colSignal AND colSignal < block_right(9) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 24
			ELSIF(block_on(24) = '1' AND block_left(10) < colSignal AND colSignal < block_right(10) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 25
			ELSIF(block_on(25) = '1' AND block_left(11) < colSignal AND colSignal < block_right(11) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 26
			ELSIF(block_on(26) = '1' AND block_left(12) < colSignal AND colSignal < block_right(12) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
			  -- block 27
			ELSIF(block_on(27) = '1' AND block_left(13) < colSignal AND colSignal < block_right(13) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 28
			ELSIF(block_on(28) = '1' AND block_left(14) < colSignal AND colSignal < block_right(14) AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= "00000110";
				  blue_m <= (OTHERS => '0');
		  
		  
		  
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
