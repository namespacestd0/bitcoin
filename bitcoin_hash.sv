module bitcoin_hash(input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data);
	
	enum logic [3:0]{IDLE, S0, S1, S2, S3, S4, S5, S6, S7, S8} state;
	
	logic [15:0] read_address, write_address; // NEXT read or write address
	parameter NUM_NONCES = 16;
	parameter int k[0:63] = '{ // k constants
		32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
		32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
		32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
		32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
		32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
		32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
		32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
		32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2};	
	parameter int h_const[0:7] = '{
		32'h6a09e667,32'hbb67ae85,32'h3c6ef372,32'ha54ff53a,32'h510e527f,32'h9b05688c,32'h1f83d9ab,32'h5be0cd19};
	logic [31:0] w[32][NUM_NONCES], AtoH[8][NUM_NONCES], h[8][NUM_NONCES], h0[8]; 
	logic [31:0] m16, m17, m18; 	// mem[16-18] backup
	logic [31:0] t0[NUM_NONCES]; //temporary values for A-H computation

	logic [1:0] round; // round is 0,1,2
	logic [7:0] t, t_plus_one; // Computation cycle counter range from 0 to 63+1

	logic commonOp;

	function logic [31:0] wtnew(input logic [31:0] w1, w14, w0, w9);
		logic [31:0] s0, s1;
			s0 = rightrotate(w1,7)^rightrotate(w1,18)^(w1>>3);
			s1 = rightrotate(w14,17)^rightrotate(w14,19)^(w14>>10);
			wtnew = w0 + s0 + w9 + s1;
	endfunction
	
	function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, t0);
		logic [31:0] s1, s0, ch, maj, t1, t2; // internal signals
		begin
			s1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
			ch = (e & f) ^ ((~e) & g);
			t1 = s1 + ch + t0;
			s0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
			maj = (a & b) ^ (a & c) ^ (b & c);
			t2 = s0 + maj;
			sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
		end
	endfunction

	function logic [31:0] rightrotate(input logic [31:0] x, input logic [7:0] r);
		begin
			rightrotate = (x >> r) | (x << (32-r));
		end
	endfunction
	
	assign mem_clk = clk;

	always_ff @(posedge clk, negedge reset_n) begin
		if (!reset_n) begin
			state <= IDLE;
		end else begin
			if (commonOp) begin
				// ** Main Operation **
				for (int n = 0; n < NUM_NONCES; n++) 
					{AtoH[0][n],AtoH[1][n],AtoH[2][n],AtoH[3][n],AtoH[4][n],AtoH[5][n],AtoH[6][n],AtoH[7][n]} <= sha256_op(AtoH[0][n],AtoH[1][n],AtoH[2][n],AtoH[3][n],AtoH[4][n],AtoH[5][n],AtoH[6][n],AtoH[7][n],t0[n]);
				// ** t0 Precomputation**
				for (int n = 0; n < NUM_NONCES; n++) 
					t0[n]		<= AtoH[6][n] + k[t_plus_one] + w[15][n];
				// ** wt PreComputation **
				for (int n = 0; n < NUM_NONCES; n++) 
					for (int i = 0; i < 15; i++) 
						w[i][n]	 <= w[i+1][n];
							
				if (round == 0 && t<14) 
					w[15][0] <= mem_read_data;
				else if (round !=0 && t<14) begin
					for (int n = 0; n < NUM_NONCES; n++) 
						w[15][n] <= w[16][n];
				end else begin 
					for (int n = 0; n < NUM_NONCES; n++) 
						w[15][n] <= wtnew(w[1][n],w[14][n],w[0][n],w[9][n]);
				end
				for (int n = 0; n < NUM_NONCES; n++) 
					for (int m = 16; m < 31; m++) 
						w[m][n] <= w[m+1][n];

			end 
			case (state)
			IDLE: begin
				mem_we			<= 0;
				// n				<= 0;
				for (int i=0; i<8; i++) h[i][0] <= h_const[i];
				if (start) begin
					state 			<= S0;
					// ** Request mem[0]
					mem_addr 		<= message_addr; 
					// ** Prepare for future
					read_address	<= message_addr + 1;
					write_address	<= output_addr;
					done			<= 0;
				end else begin
					done			<= 1;
				end
			end

			S0: begin
				state			<= S1;
				// ** Request mem[1]
				mem_addr 		<= read_address; 
				read_address	<= read_address + 1;
			end

			S1: begin
				state			<= S2;
				commonOp		<= 0;
				t_plus_one		<= 0;
				// ** Request mem[2]
				mem_addr 		<= read_address; 
				read_address	<= read_address + 1;
				// ** Read mem[0]
				for (int n = 0; n < NUM_NONCES; n++) 
					for (int i = 0; i < 15; i++) 
						w[i][n]	 <= w[i+1][n];
				w[15][0] <= mem_read_data;
			end

			S2: begin 
				state			<= S3;
				commonOp		<= 1;
				t				<= t_plus_one;
				t_plus_one		<= t_plus_one + 1;
				round			<= 0;
				// ** Request mem[3]
				mem_addr 		<= read_address; 
				read_address	<= read_address + 1;
				// ** Read mem[1]
				for (int n = 0; n < NUM_NONCES; n++) 
					for (int i = 0; i < 15; i++) 
						w[i][n]	 <= w[i+1][n];
				w[15][0] 		<= mem_read_data;
				// ** Precompute t0 of 0
				t0[0]				<= h[7][0] + k[0] + w[15][0];
				// ** Initialize A-H from h0-7
				for(int i=0;i<8;i++) AtoH[i][0] <= h[i][0];
			end

			S3: begin
				if (t==63) begin
					state 			<= S4;
					commonOp		<= 0;
					t_plus_one		<= 0;
				end else begin
					// ** t++
					t				<= t_plus_one;
					t_plus_one		<= t_plus_one + 1;
				end
				// ** Request new mem[read_address]
				mem_addr 		<= read_address; 
				read_address	<= read_address + 1;
				// ** Backup w[16-18]
				if (t_plus_one == 15) m16 <= mem_read_data;
				if (t_plus_one == 16) m17 <= mem_read_data;
				if (t_plus_one == 17) m18 <= mem_read_data;
			end

			S4: begin // ** major transition
				state 			<= S5;
				commonOp		<= 1;
				round			<= 1;
				t				<= t_plus_one;
				t_plus_one		<= t_plus_one + 1;
				for (int n = 0; n < NUM_NONCES; n++) begin
				// ** fill w values
					w[14][n]				<= m16;
					w[15][n]				<= m17;
					w[16][n]				<= m18;
					w[17][n]				<= n;						
					w[18][n]				<= 32'h80000000;
					for(int i=19;i<29;i++) w[i][n] <= 32'h00000000;
					w[29][n] 				<= 32'd640;
				// ** update h0-7 and save a copy
					for(int i=0;i<8;i++) AtoH[i][n] <= h[i][0] + AtoH[i][0];	// A-H
					// for(int i=0;i<8;i++) h0[i] <= h[i] + AtoH[i];	// extra save
					for(int i=0;i<8;i++) h[i][n] <= h[i][0] + AtoH[i][0];	// regular update
				// ** Precompute t0 of 0
					t0[n]					<= h[7][0] + AtoH[7][0] + k[0] + m16;
				end
			end

			S5: begin
				if (t==63) begin
					state 			<= S6;
					commonOp		<= 0;
					t_plus_one		<= 0;
					
				end else begin
					// ** t++
					t				<= t_plus_one;
					t_plus_one		<= t_plus_one + 1;
				end
			end

			S6: begin // ** major transition
				state 			<= S7;
				commonOp	<= 1;
				round			<= 2;
				t				<= t_plus_one;
				t_plus_one		<= t_plus_one + 1;
				for (int n = 0; n < NUM_NONCES; n++) begin
				// ** fill w values
					for(int i=0;i<8;i++)  w[i+14][n] <= h[i][n] + AtoH[i][n];
					w[22][n]			<= 32'h80000000;				
					for(int j=23;j<29;j++) w[j][n] <= 32'h00000000;
					w[29][n] 			<= 32'd256;
				// ** update h0-7 nad A-H
					for (int i=0; i<8; i++) h[i][n] <= h_const[i];
					for (int i=0; i<8; i++) AtoH[i][n] <= h_const[i];
				// ** Precompute t0 of 0
					t0[n]				<= h_const[7] + k[0] + h[0][n] + AtoH[0][n];
				end
			end

			S7: begin
				if (t==63) begin
					state 			<= S8;
					commonOp		<= 0;
					t		<= 0;
				end else begin
					// ** t++
					t				<= t_plus_one;
					t_plus_one		<= t_plus_one + 1;
				end
			end

			S8: begin
				if (t==NUM_NONCES-1)
					state 			<= IDLE;
				mem_we			<= 1;
				mem_addr		<= write_address;
				t				<= t + 1;
				write_address	<= write_address + 1;
				mem_write_data	<= h[0][t] + AtoH[0][t];
			end
		endcase
		end
	end
endmodule