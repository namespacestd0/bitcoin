module simplified_sha256(input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data);
	
	enum logic [3:0]{IDLE, R0, R1, C1, C2, A1, A2, A3, B1, B2, B3, W} state;
	
	logic [15:0] read_address, write_address; // Simply a copy of the input addressed
	logic [4:0] read_count, write_count; // read, write address offset, cycle tracker
	
	parameter int k[0:63] = '{
		32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
		32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
		32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
		32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
		32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
		32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
		32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
		32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
	};	
	logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7; 	// round-end computation value
	logic [31:0] w[16]; // the latest 16 w values
	logic [31:0] A, B, C, D, E, F, G, H;

	logic [31:0] S1, S0, t0, t1, t2, ch, s1ch, ktw, maj, h7k0, write_data; //temporary values for A-H computation

	logic	is1stRound = 1; //
	logic [5:0] t, t_plus_one; // Computation cycle counter range from 0 to 63
	
	assign mem_clk = clk;


	function logic [31:0] rrot(input logic [31:0] x, input logic [7:0] r);
		rrot = (x >> r) | (x << (32-r));
	endfunction
	
	function logic [31:0] wtnew; // function with no inputs
		logic [31:0] s0, s1;
			s0 = rrot(w[1],7)^rrot(w[1],18)^(w[1]>>3);
			s1 = rrot(w[14],17)^rrot(w[14],19)^(w[14]>>10);
			wtnew = w[0] + s0 + w[9] + s1;
	endfunction
	
	function logic [255:0] opAtoH;
		S1 					= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
		ch 					= (E & F) ^ ((~E) & G);
		S0 					= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
		maj 					= (A & B) ^ (A & C) ^ (B & C);
		t1 					= S1 + ch + t0;
		t2 					= S0 + maj;
		opAtoH = {t1 + t2, A, B, C, D + t1, E, F, G};
	endfunction

	always_ff @(posedge clk, negedge reset_n) begin
		if (!reset_n) begin
			state <= IDLE;
		end else 
			case (state)
				IDLE:
					begin
						// reset
						done				<= 1;
						mem_we			<= 0;
						h0 <= 32'h6a09e667;
						h1 <= 32'hbb67ae85;
						h2 <= 32'h3c6ef372;
						h3 <= 32'ha54ff53a;
						h4 <= 32'h510e527f;
						h5 <= 32'h9b05688c;
						h6 <= 32'h1f83d9ab;
						h7 <= 32'h5be0cd19;
						if (start) begin
							state 			<= R0;
							done				<= 0;
							// **request mem[0]**
							read_address	<= message_addr + 1;
							mem_addr 		<= message_addr; 
							// save write address
							write_address	<= output_addr;
						end
					end
				R0:
					begin
						state					<= R1;
						// initialize A-H
						A <= h0;
						B <= h1;
						C <= h2;
						D <= h3;
						E <= h4;
						F <= h5;
						G <= h6;
						H <= h7;
						// **request mem[1]**
						mem_addr 			<= read_address;
						read_address		<= read_address + 1;
					end

				R1:
					begin
						state					<= C1;
						// **request mem[2]**
						mem_addr 			<= read_address;
						read_address		<= read_address + 1;
						// **read mem[0] and prepare next index**
						w[0]					<= mem_read_data;
						read_count			<= 1;
						// **pre-compute t0 of 0**
						t0 					<= H + k[0] + mem_read_data;
						t						<= 0;
					end
					
				C1:
					begin
						if (is1stRound && read_address == 16)
							state				<= A1;
						else if (!is1stRound && t == 2)
							state				<= B1;
						// **request mem[3-16] or mem[19-21] for second round**
						mem_addr 			<= read_address;
						read_address		<= read_address + 1;
						// **read mem[1-14] or mem[1-3] for second round**
						w[read_count]		<= mem_read_data;
						read_count			<= read_count + 1;
						// **t0 of 1-14 or 1-3 for second round**
						t0 					<= G + k[read_count] + mem_read_data;
						t						<= t + 1;
						// **t1 of 0-13 or 0-2 for second round**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
					end

				A1:
					begin // t = 14 here read_count = 15 here
						state					<= C2;
						// **read mem[15] to 14 because of upcoming rotation**
						w[14]					<= mem_read_data;
						read_count			<= 0;
						// **w[16]** t = 14 here
						for (int n = 0; n < 14; n++) w[n] <= w[n+1];
						w[15] 				<= wtnew();
						// **t0 of 15**
						t0 					<= G + k[read_count] + mem_read_data;
						t						<= read_count; // t = 15
						t_plus_one			<= 16;
						// **t1 of 14**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
					end
					
				B1:
					begin
						if (t == 13)
							state				<= C2;
						// use read_count to help track of t+1, starts from 4
						read_count			<= read_count + 1;
						//  **t0 of 4-14**
						t0						<= G + k[read_count] + w[read_count];
						t						<= read_count;
						t_plus_one			<= 15;
						//  **t1 of 3-13**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
					end
				
				C2:
					begin
						if (t == 61) begin
							if (is1stRound)
								state				<= A2;
							else
								state				<= B2;
						end
						t_plus_one			<= t_plus_one + 1;
						// **w[17/16-65]**
						for (int n = 0; n < 15; n++) w[n] <= w[n+1];
						w[15] 				<= wtnew();
						//  **t0 of 16/15-64**
						t0						<= G + k[t_plus_one] + w[15];
						t						<= t_plus_one;
						//  **t1 of 15/14-63**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
				end
				
				A2:
					begin // t = 62
						state				<= A3;
						is1stRound		<= 0;
						t					<= t_plus_one;
						//  **t0 of 63**						
						t0					<= G + k[t_plus_one] + w[15];
						// **request mem[17]**
						mem_addr			<= read_address;
						read_address	<= read_address + 1;
						// **t1 of 62 modified**
						S1 				= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
						ch 				= (E & F) ^ ((~E) & G);
						S0 				= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
						maj 				= (A & B) ^ (A & C) ^ (B & C);
						t1 				= S1 + ch + t0;
						t2 				= S0 + maj;
						{A, B, C, D, E, F, G, H}  = {t1 + t2, A, B, C + h4, D + t1, E, F, G};	
						// pre-compute h7+k0 for next cycle
						h7k0				<= h7 + k[0];
					end
					
				A3:
					begin // t = 63
						state				<= C1;
						t					<= 0;						
						w[4]				<= 32'h80000000;
						w[5]		 		<= 32'h00000000;
						w[6]		 		<= 32'h00000000;
						w[7]		 		<= 32'h00000000;
						w[8]		 		<= 32'h00000000;
						w[9]		 		<= 32'h00000000;
						w[10]		 		<= 32'h00000000;
						w[11]		 		<= 32'h00000000;
						w[12]		 		<= 32'h00000000;
						w[13]		 		<= 32'h00000000;
						w[14]		 		<= 32'h00000000;
						w[15] 			<= 32'd640;					
						// **request mem[18]**
						mem_addr			<= read_address;
						read_address	<= read_address + 1;	
						// **read mem[16] to w[0]**
						w[read_count]	<= mem_read_data;
						read_count		<= read_count + 1;
						// **t0 of 0 for second round**
						t0					<= G + h7k0 + mem_read_data;	
						// **t1 of 63 modified**
						S1 				= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
						ch 				= (E & F) ^ ((~E) & G);
						S0 				= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
						maj 				= (A & B) ^ (A & C) ^ (B & C);
						t1 				= S1 + ch + t0;
						t2 				= S0 + maj + h0;
						A					<= t1 + t2;
						B					<= h1 + A; 
						C					<= h2 + B; 
						D					<= h3 + C; 
						E					<= D + t1; 
						F					<= h5 + E; 
						G					<= h6 + F; 
						H					<= h7 + G;	
						h0				<= t1 + t2;
						h1				<= h1 + A; 
						h2				<= h2 + B; 
						h3				<= h3 + C; 
						h4				<= D + t1; 
						h5				<= h5 + E; 
						h6				<= h6 + F; 
						h7				<= h7 + G;						
					end
				
				B2:
					begin // t = 62
						state				<= B3;
						t					<= t_plus_one;
						//  **t0 of 63**						
						t0					<= G + k[t_plus_one] + w[15];
						// **t1 of 62 modified**
						S1 				= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
						ch 				= (E & F) ^ ((~E) & G);
						S0 				= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
						maj 				= (A & B) ^ (A & C) ^ (B & C);
						t1 				= S1 + ch + t0;
						t2 				= S0 + maj;
						{A, B, C, D, E, F, G, H}  = {t1 + t2, A, B, C + h4, D + t1, E, F, G};											
					end
					
				B3:
					begin // t = 63
						state				<= W;
						t					<= 0;									
						// **t1 of 63 modified**
						S1 				= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
						ch 				= (E & F) ^ ((~E) & G);
						S0 				= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
						maj 				= (A & B) ^ (A & C) ^ (B & C);
						t1 				= S1 + ch + t0;
						t2 				= S0 + maj + h0;
//						A					<= t1 + t2 + h0;
//						B					<= h1 + A; 
//						C					<= h2 + B; 
//						D					<= h3 + C; 
//						E					<= D + t1; 
//						F					<= h5 + E; 
//						G					<= h6 + F; 
//						H					<= h7 + G;	
//						h0				<= t1 + t2 + h0;
						h1				<= h1 + A; 
						h2				<= h2 + B; 
						h3				<= h3 + C; 
						h4				<= D + t1; 
						h5				<= h5 + E; 
						h6				<= h6 + F; 
						h7				<= h7 + G;
						// *write attemp mem[write_address] = h[write_count]*
						mem_addr 		<= write_address;
						mem_write_data	<= t1 + t2;
						mem_we			<= 1;	
						// *next write source index and destination update*
						write_address		<= write_address + 1;
						write_count			<= 1;							
					end
				
				W:
					begin
						if (write_count == 7) begin
							state 			<= IDLE;
						end
						// *write attemp mem[write_address] = h[write_count]*
						mem_addr 			<= write_address;
						case(write_count)
							1:
								write_data = h1;
							2:
								write_data = h2;
							3:
								write_data = h3;
							4:
								write_data = h4;
							5:
								write_data = h5;
							6:
								write_data = h6;
							7:
								write_data = h7;
						endcase
						mem_write_data		<= write_data;
						// *next write source index and destination update*
						write_address		<= write_address + 1;
						write_count			<= write_count + 1;	
					end
			endcase
	end
 
endmodule