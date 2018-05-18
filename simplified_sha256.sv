module simplified_sha256(input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data);
	
	enum logic [3:0]{IDLE, R0, R1, C1, C2, C3, R2, R3, W} state;
	
	logic [15:0] read_address, write_address; // Simply a copy of the input addressed
	logic [15:0] read_count = 0, write_count = 0; // read, write address offset, cycle tracker
	
	logic [31:0] h[8];	// round-end computation value
	logic [31:0] w[16]; // the latest 16 w values
	logic [31:0] A, B, C, D, E, F, G, H;
//	logic [31:0] A0, B0, C00, D0, E0, F0, G0, H0;

	logic [31:0] S1, S0, t0, t1, t2, ch, s1ch, ktw, maj; //temporary values for A-H computation

	logic	is1stRound = 1; //
	logic [15:0] t = 0; // Computation cycle counter range from 0 to 63
	
	assign mem_clk = clk;

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
	function logic [31:0] rrot(input logic [31:0] x, input logic [7:0] r);
		rrot = (x >> r) | (x << (32-r));
	endfunction
	

	
	function logic [31:0] wtnew; // function with no inputs
		logic [31:0] s0, s1;
			s0 = rrot(w[1],7)^rrot(w[1],18)^(w[1]>>3);
			s1 = rrot(w[14],17)^rrot(w[14],19)^(w[14]>>10);
			wtnew = w[0] + s0 + w[9] + s1;
	endfunction
	function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
		logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals

		 S1 = rrot(e, 6) ^ rrot(e, 11) ^ rrot(e, 25);
		 ch = (e & f) ^ ((~e) & g);
		 t1 = h + S1 + ch + k[t] + w;
		 S0 = rrot(a, 2) ^ rrot(a, 13) ^ rrot(a, 22);
		 maj = (a & b) ^ (a & c) ^ (b & c);
		 t2 = S0 + maj;
//		 $display("%x %x %x", t2, S0, maj);

		 sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
		 
	endfunction
	always_ff @(posedge clk, negedge reset_n) begin
		if (!reset_n) begin
			state <= IDLE;
		end else 
			case (state)
				
				IDLE:
					if (start) begin
						state 				<= R0;
						read_address		<= message_addr;
						write_address		<= output_addr;
						done					<= 0;
						mem_we				<= 0;
						mem_addr 			<= message_addr;
						h[0] = 32'h6a09e667;
						h[1] = 32'hbb67ae85;
						h[2] = 32'h3c6ef372;
						h[3] = 32'ha54ff53a;
						h[4] = 32'h510e527f;
						h[5] = 32'h9b05688c;
						h[6] = 32'h1f83d9ab;
						h[7] = 32'h5be0cd19;
						A <= 32'h6a09e667;
						B <= 32'hbb67ae85;
						C <= 32'h3c6ef372;
						D <= 32'ha54ff53a;
						E <= 32'h510e527f;
						F <= 32'h9b05688c;
						G <= 32'h1f83d9ab;
						H <= 32'h5be0cd19;
						read_count		<= read_count + 1;
					end
					
				R0:
					begin
						state				<= R1;
						read_count		<= read_count + 1;
						mem_addr			<= read_address + read_count;
						
					end
					
				R1:
					begin
						if (read_count == 18) begin
							// t = 16 for this section
							// w[15] is w of t=15 to start with
							state 			<= C1;
							for (int n = 0; n < 15; n++) w[n] <= w[n+1];
							w[15] <= wtnew();
						end else begin
							state 			<= R1;
							w[read_count-2]<= mem_read_data;
							read_count		<= read_count + 1;
							mem_addr			<= read_address + read_count;
							{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, mem_read_data, t);
							t					<= t + 1;
						end
					end
					
				C1:
					begin
							state 			<= C2;
							// w[15] is w of t=16 to start with
							for (int n = 0; n < 15; n++) w[n] <= w[n+1];
							w[15] 			<= wtnew();
							// t = 16 for this section
							t0 				<= G + k[t] + w[15];
					end
				
				C2:
					begin
						if (t == 64) 
							state				<= C3;
						else begin
							// t = 16 the first time this section runs
							// w[15] is w of t=17 to start with the first time this section runs
							for (int n = 0; n < 15; n++) w[n] <= w[n+1];
							w[15] 			<= wtnew();
							// pre-comptue t = 17 the first time this runs
							t0					<=  G + k[t+1] + w[15];
							//
							S1 				= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
							ch 				= (E & F) ^ ((~E) & G);
							S0 				= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
							maj 				= (A & B) ^ (A & C) ^ (B & C);
							t1 				= S1 + ch + t0;
							t2 				= S0 + maj;
							//  k
							t					<= t + 1;
							{A, B, C, D, E, F, G, H} 	<= {t1 + t2, A, B, C, D + t1, E, F, G};
						end
					end
							
				C3:
					begin
						if (is1stRound) begin
							state 		<= R2;
							w[4]			<= 32'h80000000;
							w[5]		 	<= 32'h00000000;
							w[6]		 	<= 32'h00000000;
							w[7]		 	<= 32'h00000000;
							w[8]		 	<= 32'h00000000;
							w[9]		 	<= 32'h00000000;
							w[10]		 	<= 32'h00000000;
							w[11]		 	<= 32'h00000000;
							w[12]		 	<= 32'h00000000;
							w[13]		 	<= 32'h00000000;
							w[14]		 	<= 32'h00000000;
							w[15] 		<= 32'd640;
							is1stRound	<= 0;
							mem_we		<= 0;
							mem_addr 	<= read_address + read_count - 2;
							t					<= 0;
							A					<= h[0] + A;
							B					<= h[1] + B; 
							C					<= h[2] + C; 
							D					<= h[3] + D; 
							E					<= h[4] + E; 
							F					<= h[5] + F; 
							G					<= h[6] + G; 
							H					<= h[7] + H;
							read_count		<= read_count - 1;
						end else begin
							state			<= W;
						end
						h[0]				<= h[0] + A;
						h[1]				<= h[1] + B;
						h[2]				<= h[2] + C;
						h[3]				<= h[3] + D;
						h[4]				<= h[4] + E;
						h[5]				<= h[5] + F;
						h[6]				<= h[6] + G;
						h[7]				<= h[7] + H;
					end
					//asdfasdfasdfasdasdf
					
				R2:
					begin
						state				<= R3;
						read_count		<= read_count + 1;
						mem_addr			<= read_address + read_count;	
					end
					
				R3:
					begin
						if (read_count == 34) begin
							state 			<= C1;
							for (int n = 0; n < 15; n++) w[n] <= w[n+1];
							w[15] <= wtnew();
						end else begin
							state 			<= R3;
							if (read_count < 22)
								w[read_count-18]<= mem_read_data;
							read_count		<= read_count + 1;
							mem_addr			<= read_address + read_count;
							if (t < 4)
								{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, mem_read_data, t);
							else
								{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[t], t);
							t					<= t + 1;
						end
					end
				
				W:
					if (write_count == 16) begin
						state 			<= IDLE;
						done				<= 1;
					end else begin
						mem_addr 		<= write_address + write_count;
						mem_we			<= 1;
						mem_write_data	<= h[write_count];
						write_count		<= write_count + 1;	
					end
			endcase
	end
 
endmodule