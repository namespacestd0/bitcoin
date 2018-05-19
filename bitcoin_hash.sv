module bitcoin_hash(input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data);
	
	enum logic [3:0]{IDLE, R0, R1, A1, A2, B1, B2, C1, C2, C3, SHA256} state, nextState;
	
	logic [15:0] read_address, write_address; // NEXT read or write address
	logic [15:0] read_to, write_from; // Next read of write index or counter
	
	parameter int k[0:63] = '{ // k constants
		32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
		32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
		32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
		32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
		32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
		32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
		32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
		32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2};	
	logic [31:0] h[8]; 	// round-end computation value
	logic [31:0] w[16]; // the latest 16 w values
	logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7; 	// round1-end computation value
	logic [31:0] m16, m17, m18; 	// mem[16-18] backup
	logic [31:0] A, B, C, D, E, F, G, H;

	logic [31:0] s1, s0, t0, t1, t2, ch, s1ch, ktw, maj, a, b; //temporary values for A-H computation

	logic [1:0] round; // round is 0,1,2
	logic [15:0] t; // Computation cycle counter range from 0 to 63
	logic [15:0] t_plus_one; // performance-wise t+1 tracking register
	logic [3:0] n; // nonce range from 0 to 15

	logic sw_mem_read;	// request mem[read_address], w[read_to]=mem_read_data
	logic sw_wt_update;	// update w[0-15]

	logic [1:0] sw_t0;	// pre-compute t0 of t from mem_read_data, w[t+1] or w[15]
	logic [31:0] wt;	// switching wire for above
	// logic [15:0] last_t; // break out condition
	
	function logic [31:0] rrot(input logic [31:0] x, input logic [7:0] r);
		rrot = (x >> r) | (x << (32-r));
	endfunction
	
	function logic [31:0] wtnew;
		logic [31:0] s0, s1;
			s0 = rrot(w[1],7)^rrot(w[1],18)^(w[1]>>3);
			s1 = rrot(w[14],17)^rrot(w[14],19)^(w[14]>>10);
			wtnew = w[0] + s0 + w[9] + s1;
	endfunction

	function logic [127:0] opAtoH_bitOp; // compute s1, s0, ch, maj
		logic [31:0] s1, ch, s0, maj;
			s1 					= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
			ch 					= (E & F) ^ ((~E) & G);
			s0 					= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
			maj 				= (A & B) ^ (A & C) ^ (B & C);
			opAtoH_bitOp		= {s1, ch, s0, maj};
	endfunction
	
	function logic [255:0] opAtoH;
		{s1, ch, s0, maj} 	= opAtoH_bitOp();
		t1 					= s1 + ch + t0;
		t2 					= s0 + maj;
		opAtoH = {t1 + t2, A, B, C, D + t1, E, F, G};
	endfunction

	assign mem_clk = clk;
	always_ff @(posedge clk, negedge reset_n) begin
		if (!reset_n) begin
			state <= IDLE;
		end else 
			case (state)
				IDLE:
					begin
						// reset
						mem_we			<= 0;
						h[0] <= 32'h6a09e667;
						h[1] <= 32'hbb67ae85;
						h[2] <= 32'h3c6ef372;
						h[3] <= 32'ha54ff53a;
						h[4] <= 32'h510e527f;
						h[5] <= 32'h9b05688c;
						h[6] <= 32'h1f83d9ab;
						h[7] <= 32'h5be0cd19;
						// switch approach
						sw_mem_read 	<= 0;
						sw_wt_update 	<= 0;
						sw_t0	 		<= 0;

						if (start) begin
							state 			<= R0;
							done			<= 0;
							n				<= 0;
							// **request mem[0]**
							read_address	<= message_addr + 1;
							mem_addr 		<= message_addr; 
							// save write address
							write_address	<= output_addr;
						end else begin
							done			<= 1;
						end
					end
				R0:
					begin
						state					<= R1;
						// initialize A-H
						A <= h[0];
						B <= h[1];
						C <= h[2];
						D <= h[3];
						E <= h[4];
						F <= h[5];
						G <= h[6];
						H <= h[7];
						a <= k[0] + h[7]; // optimization
						// **request mem[1]**
						mem_addr 			<= read_address;
						read_address		<= read_address + 1;
						round 				<= 0;
					end

				R1:
					begin
						state					<= SHA256;
						sw_mem_read				<= 1;
						sw_wt_update			<= 0;
						sw_t0					<= 1; // from mem_read
						// **request mem[2]**
						mem_addr 				<= read_address;
						read_address			<= read_address + 1;
						// **read mem[0] and prepare next index**
						w[0]					<= mem_read_data;
						read_to					<= 1;
						// **pre-compute t0 of 0**
						t0 						<= mem_read_data + a;
						t						<= 0;
						t_plus_one				<= 1;
					end
					
				SHA256:
					begin
						// state transition
						if (t==1) begin
							if (n==0 && round==1) begin
								sw_mem_read			<= 0;
								sw_wt_update		<= 0;
								sw_t0				<= 1; // base on w[t+1]
							end
						end else if (t==13) begin
							if (n==0 && round==0)
								state				<= A1;
							sw_mem_read			<= 0;
							sw_wt_update		<= 1;
							sw_t0				<= 3; // base on w[15]
						end else if (t==63) begin
							if (n==0 && round==0) begin
								state				<= A2;
								mem_addr 			<= read_address;
								read_address		<= read_address + 1;
							end else if (n==0 && round==1) begin
								state				<= B2;							
							end else if (round==1) // n>0
								state				<= C2;
							else // round = 2
								state				<= C3;
						end
						//
						if (sw_mem_read) begin
							mem_addr 			<= read_address;
							read_address		<= read_address + 1;
							w[read_to]			<= mem_read_data;
							read_to				<= read_to + 1;
						end
						
						if (sw_wt_update) begin
							for (int n = 0; n < 15; n++) w[n] <= w[n+1];
							w[15] 				<= wtnew();
						end

						if (sw_t0) begin
							if (sw_t0 == 1)
								wt					= mem_read_data;
							else if (sw_t0 == 2)
								wt					= w[t_plus_one];
							else
								wt					= w[15];
							// pre compute t0 of t+1 with w of t+1 selected above 
							t0					<= G + k[t_plus_one] + wt;
							t					<= t_plus_one;
							t_plus_one			<= t_plus_one + 1;
						end

						{A, B, C, D, E, F, G, H}<= opAtoH();

					end

				A1:
					begin // t = 14 here read_to = 15 here
						state					<= SHA256;
						sw_mem_read				<= 0;
						sw_wt_update			<= 1;
						sw_t0					<= 3; // from w[15]				
						// **read mem[15] to 14 because of upcoming rotation**
						w[t]					<= mem_read_data;
						read_to			<= 0;
						// **w[16]** t = 14 here
						for (int n = 0; n < 14; n++) w[n] <= w[n+1];
						w[15] 				<= wtnew();
						// **t0 of 15**
						t0 					<= G + k[read_to] + mem_read_data;
						t						<= read_to; // t = 15
						t_plus_one			<= 16;
						// **t1 of 14**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
					end
					
				A2:
					begin
						state				<= B1;
						round				<= 1;
						a = h[7] + H;
						b = k[0] + mem_read_data;
						h[0]				<= h[0] + A;
						h[1]				<= h[1] + B;
						h[2]				<= h[2] + C;
						h[3]				<= h[3] + D;
						h[4]				<= h[4] + E;
						h[5]				<= h[5] + F;
						h[6]				<= h[6] + G;
						h[7]				<= a;							
						h0					<= h[0] + A;
						h1					<= h[1] + B;
						h2					<= h[2] + C;
						h3					<= h[3] + D;
						h4					<= h[4] + E;
						h5					<= h[5] + F;
						h6					<= h[6] + G;
						h7					<= a;						
						A					<= h[0] + A;
						B					<= h[1] + B; 
						C					<= h[2] + C; 
						D					<= h[3] + D; 
						E					<= h[4] + E; 
						F					<= h[5] + F; 
						G					<= h[6] + G; 
						H					<= a;
						w[3]				<= 32'h00000000;						
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
						// **request mem[17]**
						mem_addr			<= read_address;
						read_address	<= read_address + 1;
						// **read mem[16] to w[0]**
						w[read_to]	<= mem_read_data;
						read_to		<= read_to + 1;
						// **t0 of 0 for second round**
						t0					<= a + b;
						t					<= 0;
						t_plus_one			<= 1;		
						// backup mem[16/0]
						m16					<= mem_read_data;
					end
					
				B1:
					begin
						if (!round && read_address == 16)
							state				<= A1;
						else if (round && t == 1) begin
							state					<= SHA256;
							sw_mem_read				<= 0;
							sw_wt_update			<= 0;
							sw_t0					<= 2; // from mem[t+1]
						end
							// state				<= B1;
						// **request mem[3-16] or mem[19-21] for second round**
						mem_addr 			<= read_address;
						read_address		<= read_address + 1;
						// **read mem[1-14] or mem[1-3] for second round**
						w[read_to]		<= mem_read_data;
						read_to			<= read_to + 1;
						// **t0 of 1-14 or 1-3 for second round**
						t0 					<= G + k[read_to] + mem_read_data;
						t					<= t + 1;
						t_plus_one			<= t_plus_one + 1;
						// **t1 of 0-13 or 0-2 for second round**
						{A, B, C, D, E, F, G, H} 	<= opAtoH();
						// backup mem17 and mem18
						if (round==1)
							if (read_to==1)
								m17					<= mem_read_data;
							else if (read_to==2)
								m18					<= mem_read_data;
					end

				B2:
					begin
						// state					<= B1;
						state					<= SHA256;
						sw_mem_read				<= 0;
						sw_wt_update			<= 0;
						sw_t0					<= 2; // from mem[t+1]
						//
						round 					<= 2;
						h[0]					<= 32'h6a09e667;
						h[1]					<= 32'hbb67ae85;
						h[2]					<= 32'h3c6ef372;
						h[3]					<= 32'ha54ff53a;
						h[4]					<= 32'h510e527f;
						h[5]					<= 32'h9b05688c;
						h[6]					<= 32'h1f83d9ab;
						h[7]					<= 32'h5be0cd19;	
						A 						<= 32'h6a09e667;
						B 						<= 32'hbb67ae85;
						C 						<= 32'h3c6ef372;
						D 						<= 32'ha54ff53a;
						E 						<= 32'h510e527f;
						F 						<= 32'h9b05688c;
						G 						<= 32'h1f83d9ab;
						H 						<= 32'h5be0cd19;
						// w0-15 from previous result and word expansion						
						w[0]					<= h[0] + A;
						w[1]					<= h[1] + B;
						w[2]					<= h[2] + C;
						w[3]					<= h[3] + D;
						w[4]					<= h[4] + E;
						w[5]					<= h[5] + F;
						w[6]					<= h[6] + G;
						w[7]					<= h[7] + H;	
						w[8]		 			<= 32'h80000000;
						w[9]		 			<= 32'h00000000;
						w[10]		 			<= 32'h00000000;
						w[11]		 			<= 32'h00000000;
						w[12]		 			<= 32'h00000000;
						w[13]		 			<= 32'h00000000;
						w[14]		 			<= 32'h00000000;
						w[15] 					<= 32'd256;
						
						//  **t0 of 0**
						t0						<= 32'h5be0cd19 + k[0] + h[0] + A;
						t						<= 0;
						t_plus_one				<= 1;
					end
			
		
				C1: // new beginning after first round
					begin
						state				<= SHA256;
						sw_mem_read			<= 0;
						sw_wt_update		<= 0;
						sw_t0				<= 2; // base on w[t+1]
						// 
						mem_we				<= 0;
						//
						round				<= 1;
						t					<= 0;
						t_plus_one			<= 1;
						//
						h[0]				<= h0;
						h[1]				<= h1;
						h[2]				<= h2;
						h[3]				<= h3;
						h[4]				<= h4;
						h[5]				<= h5;
						h[6]				<= h6;
						h[7]				<= h7;												
						A					<= h0;
						B					<= h1; 
						C					<= h2; 
						D					<= h3; 
						E					<= h4; 
						F					<= h5; 
						G					<= h6; 
						H					<= h7;
						w[0]				<= m16;
						w[1]				<= m17;
						w[2]				<= m18;
						w[3]				<= n;					
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
						w[15] 				<= 32'd640;
						//
						t0					<= h7 + k[0] + m16;	
					end

				C2:
					begin
						state				<= SHA256;
						sw_mem_read			<= 0;
						sw_wt_update		<= 0;
						sw_t0				<= 2;
						//
						round 					<= 2;
						h[0]					<= 32'h6a09e667;
						h[1]					<= 32'hbb67ae85;
						h[2]					<= 32'h3c6ef372;
						h[3]					<= 32'ha54ff53a;
						h[4]					<= 32'h510e527f;
						h[5]					<= 32'h9b05688c;
						h[6]					<= 32'h1f83d9ab;
						h[7]					<= 32'h5be0cd19;	
						A 						<= 32'h6a09e667;
						B 						<= 32'hbb67ae85;
						C 						<= 32'h3c6ef372;
						D 						<= 32'ha54ff53a;
						E 						<= 32'h510e527f;
						F 						<= 32'h9b05688c;
						G 						<= 32'h1f83d9ab;
						H 						<= 32'h5be0cd19;
						// w0-15 from previous result and word expansion						
						w[0]					<= h[0] + A;
						w[1]					<= h[1] + B;
						w[2]					<= h[2] + C;
						w[3]					<= h[3] + D;
						w[4]					<= h[4] + E;
						w[5]					<= h[5] + F;
						w[6]					<= h[6] + G;
						w[7]					<= h[7] + H;	
						w[8]		 			<= 32'h80000000;
						w[9]		 			<= 32'h00000000;
						w[10]		 			<= 32'h00000000;
						w[11]		 			<= 32'h00000000;
						w[12]		 			<= 32'h00000000;
						w[13]		 			<= 32'h00000000;
						w[14]		 			<= 32'h00000000;
						w[15] 					<= 32'd256;
						//  **t0 of 0**
						t0						<= 32'h5be0cd19 + k[0] + h[0] + A;
						t						<= 0;
						t_plus_one				<= 1;
					end

				C3:
					begin
						if ( n == 15)
							state					<= IDLE;
						else
							state					<= C1;
						// write a result
						mem_we					<= 1;
						mem_addr				<= write_address + n;
						mem_write_data 			<= h[0] + A;
						// count nounce
						n						<= n + 1;
						// 
					end
			endcase
	end
 
endmodule
