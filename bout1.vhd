library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;


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
	
	
	block_1_left :  INTEGER := 80;
	block_1_right : INTEGER := 105;
	
	block_2_left :  INTEGER := 115;
	block_2_right : INTEGER := 140;
	
	block_3_left :  INTEGER := 150;
	block_3_right : INTEGER := 175;
	
	block_4_left :  INTEGER := 185;
	block_4_right : INTEGER := 210;
	
	block_5_left :  INTEGER := 220;
	block_5_right : INTEGER := 245;
	
	block_6_left :  INTEGER := 255;
	block_6_right : INTEGER := 280;
	
	block_7_left :  INTEGER := 290;
	block_7_right : INTEGER := 315;
	
	block_8_left :  INTEGER := 325;
	block_8_right : INTEGER := 350;
	
	block_9_left :  INTEGER := 360;
	block_9_right : INTEGER := 385;
	
	block_10_left :  INTEGER := 395;
	block_10_right : INTEGER := 420;
	
	block_11_left :  INTEGER := 430;
	block_11_right : INTEGER := 455;
	
	block_12_left :  INTEGER := 465;
	block_12_right : INTEGER := 490;
	
	block_13_left :  INTEGER := 500;
	block_13_right : INTEGER := 525;
	
	block_14_left :  INTEGER := 535;
	block_14_right : INTEGER := 560
	); 


	port ( 
		-- counter direction flip
		button_press : in std_logic; 
		
		-- internal clock
		max10_clk      :    IN STD_LOGIC; -- ALSO pixel clock for VGA mode being used

		hex5         : OUT STD_LOGIC_VECTOR(6 downto 0);
		hex4         : OUT STD_LOGIC_VECTOR(6 downto 0);
		hex3         : OUT STD_LOGIC_VECTOR(6 downto 0);
		hex2         : OUT STD_LOGIC_VECTOR(6 downto 0);
		hex1         : OUT STD_LOGIC_VECTOR(6 downto 0);
		hex0         : OUT STD_LOGIC_VECTOR(6 downto 0);
		
		
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
		blue_m     :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0') --blue magnitude output to DAC
	);

end bout1;

ARCHITECTURE breakout_1p OF bout1 IS

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
			disp_ena	:	OUT	STD_LOGIC;	--display enable ('1' = display time, '0' = blanking time)
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
signal ball_counter : INTEGER:= 1250000;
signal ball_clk : std_logic := '0';

-- ball signals
--signal ball_top : INTEGER:= 235;
--signal ball_bottom : INTEGER:= 240;
signal ball_top : INTEGER:= 145;
signal ball_bottom : INTEGER:= 150;
signal ball_left : INTEGER := 315;
signal ball_right : INTEGER := 320;

-- intialized to left and down
signal left_right : std_logic := '0'; 
signal up_down : std_logic := '1'; 
signal reset_location : std_logic := '0'; 

BEGIN

-- movement for the ball
ball_direction : process(pll_OUT_to_vga_controller_IN)
begin
	if(rising_edge(pll_OUT_to_vga_controller_IN)) then 
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
			
		 else
			reset_location <= '0';
		 end if;
	end if;
end process;

ball_movement : process(ball_clk)
begin

IF(rising_edge(ball_clk)) THEN
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
		ball_top <= 145;
		ball_bottom <= 150;
		ball_left <= 315;
		ball_right <= 320;
	end if;
END IF;
end process;	

-- 0.05 second clock for ball movement testing
ball_clock : process(max10_clk)
        begin
        if(rising_edge(max10_clk)) then 
            ball_counter <= ball_counter - 1;
            -- if count value has counted to 2,500,000, toggle ball_clock
                if (ball_counter <= 0) then
                    ball_clk <= not ball_clk;
                    ball_counter <= 1250000;
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

rotary_encoder: process(encode_clk)
		 begin
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
			ELSIF(block_1_left < colSignal AND colSignal < block_1_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			-- block 2
			ELSIF(block_2_left < colSignal AND colSignal < block_2_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 3
			ELSIF(block_3_left < colSignal AND colSignal < block_3_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 4
			ELSIF(block_4_left < colSignal AND colSignal < block_4_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 5
			ELSIF(block_5_left < colSignal AND colSignal < block_5_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 6
			ELSIF(block_6_left < colSignal AND colSignal < block_6_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 7
			ELSIF(block_7_left < colSignal AND colSignal < block_7_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 8
			ELSIF(block_8_left < colSignal AND colSignal < block_8_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 9
			ELSIF(block_9_left < colSignal AND colSignal < block_9_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 10
			ELSIF(block_10_left < colSignal AND colSignal < block_10_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 11
			ELSIF(block_11_left < colSignal AND colSignal < block_11_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 12
			ELSIF(block_12_left < colSignal AND colSignal < block_12_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 13
			ELSIF(block_13_left < colSignal AND colSignal < block_13_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 14
			ELSIF(block_14_left < colSignal AND colSignal < block_14_right AND row_one_top < rowSignal AND rowSignal < row_one_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  
			-- block 15
			ELSIF(block_1_left < colSignal AND colSignal < block_1_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			-- block 16
			ELSIF(block_2_left < colSignal AND colSignal < block_2_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 17
			ELSIF(block_3_left < colSignal AND colSignal < block_3_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 18
			ELSIF(block_4_left < colSignal AND colSignal < block_4_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 19
			ELSIF(block_5_left < colSignal AND colSignal < block_5_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 20
			ELSIF(block_6_left < colSignal AND colSignal < block_6_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 21
			ELSIF(block_7_left < colSignal AND colSignal < block_7_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
				  red_m <= (OTHERS => '1');
				  green_m  <= (OTHERS => '1');
				  blue_m <= (OTHERS => '1');
			  -- block 22
			ELSIF(block_8_left < colSignal AND colSignal < block_8_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 23
			ELSIF(block_9_left < colSignal AND colSignal < block_9_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 24
			ELSIF(block_10_left < colSignal AND colSignal < block_10_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 25
			ELSIF(block_11_left < colSignal AND colSignal < block_11_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 26
			ELSIF(block_12_left < colSignal AND colSignal < block_12_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 27
			ELSIF(block_13_left < colSignal AND colSignal < block_13_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
			  red_m <= (OTHERS => '1');
			  green_m  <= (OTHERS => '1');
			  blue_m <= (OTHERS => '1');
			  -- block 28
			ELSIF(block_14_left < colSignal AND colSignal < block_14_right AND row_two_top < rowSignal AND rowSignal < row_two_bottom) THEN
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
  
end breakout_1p;