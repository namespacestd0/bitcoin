module bitcoin_hash(input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data);
	
	enum logic [3:0]{IDLE, FIRST, SECOND, THIRD, WRITE} state;
	enum logic [1:0]{PREP, COMPUTE, UPDATE, BOTH} state_mainOp;
	enum logic [1:0]{READ, WTNEW, FILL640, FILL256} state_w15;
	enum logic {INIT, COUNT} state_t;
	
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
	logic [31:0] w[32]; // the latest 16 w values plus buffer values
	logic [31:0] h0[8]; 	// round1-end computation value
	logic [31:0] m16, m17, m18; 	// mem[16-18] backup
	// logic [31:0] A, B, C, D, E, F, G, H;
	logic [31:0] AtoH[8];
	// NEW
	// logic [31:0] read_buffer;

	logic [31:0] t0; //temporary values for A-H computation
	logic [31:0] s1, ch, s0, maj; //temporary values for A-H computation

	logic [1:0] round; // round is 0,1,2
	logic [8:0] t; // Computation cycle counter range from 0 to 63+1
	logic [8:0] t_plus_one; // performance-wise t+1 tracking register
	logic [3:0] n; // nonce range from 0 to 15

	logic saved;

	logic wtread;
	// logic sw_read;		// request mem[read_address], w[read_to]=mem_read_data
	// logic sw_wtmem;		// update w[0-15]
	// logic sw_t0;		// pre-compute t0 of t from mem_read_data, or w[15]
	logic [31:0] wt;	// switching wire for above
	// logic [15:0] last_t; // break out condition
	// assign A = AtoH[0];
	// assign B = AtoH[1];
	// assign C = AtoH[2];
	// assign D = AtoH[3];
	// assign E = AtoH[4];
	// assign F = AtoH[5];
	// assign G = AtoH[6];
	// assign H = AtoH[7];
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
			s1 					= rrot(AtoH[4], 6) ^ rrot(AtoH[4], 11) ^ rrot(AtoH[4], 25);
			ch 					= (AtoH[4] & AtoH[5]) ^ ((~AtoH[4]) & AtoH[6]);
			s0 					= rrot(AtoH[0], 2) ^ rrot(AtoH[0], 13) ^ rrot(AtoH[0], 22);
			maj 				= (AtoH[0] & AtoH[1]) ^ (AtoH[0] & AtoH[2]) ^ (AtoH[1] & AtoH[2]);
			opAtoH_bitOp		= {s1, ch, s0, maj};
	endfunction
	
	function logic [255:0] opAtoH;
		logic [31:0] t1, t2;
			{s1, ch, s0, maj} 	= opAtoH_bitOp();
			t1 					= s1 + ch + t0;
			t2 					= s0 + maj;
			opAtoH = {t1 + t2, AtoH[0], AtoH[1], AtoH[2], AtoH[3] + t1, AtoH[4], AtoH[5], AtoH[6]};
	endfunction

	assign mem_clk = clk;
	always_ff @(posedge clk) begin // A-H <= ?
		case (state_mainOp)
			PREP:
				for(int i=0;i<8;i++) AtoH[i] <= h[i];
			COMPUTE:
				{AtoH[0],AtoH[1],AtoH[2],AtoH[3],AtoH[4],AtoH[5],AtoH[6],AtoH[7]} <= opAtoH();
			UPDATE:	begin											
				for(int i=0;i<8;i++) AtoH[i] <= h[i] + AtoH[i];	
				if (!saved) for(int i=0;i<8;i++) h0[i] <= h[i] + AtoH[i];
				saved <= 1;
			end
			BOTH: begin
				for(int i=0;i<8;i++) AtoH[i] <= h[i];
				
			end
		endcase
	end
	always_ff @(posedge clk) begin // w[15] <= ?
		case (state_w15)
			READ: begin
				// w[0-14]
				for (int n = 0; n < 15; n++) w[n] <= w[n+1];
				// w[15]
				if (round == 0) w[15] <= mem_read_data; // read from memory
				else for (int m = 15; m < 31; m++) w[m] <= w[m+1]; // read from buffer
			end
			WTNEW: begin
				for (int n = 0; n < 15; n++) w[n] <= w[n+1];
				w[15] 				<= wtnew();
				if (round==0) begin
					if (t_plus_one == 15) m16 <= mem_read_data;
					if (t_plus_one == 16) m17 <= mem_read_data;
					if (t_plus_one == 17) m18 <= mem_read_data;
				end			
			end
			FILL640: begin
				w[15+1]				<= m16;
				w[16+1]				<= m17;
				w[17+1]				<= m18;
				w[18+1]				<= n;						
				w[19+1]				<= 32'h80000000;
				for(int i=21;i<31;i++) w[i] <= 32'h00000000;
				w[30+1] 			<= 32'd640;			
			end
			FILL256: begin
				// for(int i=16;i<24;i++)  w[i] <= h0[i] + AtoH[i];
				w[24]				<= 32'h80000000;				
				for(int j=25;j<31;j++) w[j] <= 32'h00000000;
				w[31] 				<= 32'd256;
			end
		endcase
	end
	always_ff @(posedge clk) begin // t0 <= ?
		if (t_plus_one==0)
			if (round==1)
				if (saved)
					t0		<= h0[7] + AtoH[7] + k[t_plus_one] + w[15];
				else
					t0		<= h[7] + AtoH[7] + k[t_plus_one] + w[15];
			else
				t0		<= h[7] + k[t_plus_one] + w[15];
		else
			t0		<= AtoH[6] + k[t_plus_one] + w[15];
	end
	always_ff @(posedge clk) begin // t counter
		case (state_t)
			INIT: begin
				t			<= 63;
				t_plus_one	<= 64;
			end
			COUNT: begin
				t			<= t_plus_one;
				if (t_plus_one == 64)
					t_plus_one	<= 0;
				else t_plus_one	<= t_plus_one + 1;
			end
		endcase
	end
	always_ff @(posedge clk, negedge reset_n) begin
		if (!reset_n) begin
			state <= IDLE;
		end else case (state)
			IDLE: begin
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

				if (start) begin
					state 			<= FIRST;
					state_t			<= INIT;
					round 			<= 0;
					done			<= 0;
					n				<= 0;
					// **request mem[0]**
					mem_addr 		<= message_addr; 
					// **prepare for future**
					read_address	<= message_addr + 1;
					write_address	<= output_addr;
				end else begin
					done			<= 1;
				end
			end

			FIRST: begin
				state_t			<= COUNT;
				if (read_address == 1)
					state_w15	<= READ;
				else if (t==13)
					state_w15	<= WTNEW;
				else if (t==61) begin
					state		<= SECOND;
					state_w15	<= FILL640;
					round		<= 1;
				end else if (t==63) begin
					state_mainOp<= PREP;
				end else if (t==64) begin
					state_mainOp<= COMPUTE;
				end

				if (read_address <= 18) begin
					mem_addr 		<= read_address; 
					read_address	<= read_address + 1;
				end
			end

			SECOND: begin
				if (t==13) begin
					state_w15	<= WTNEW;
				end
				if (t==61) begin
					state		<= THIRD;
					round		<= 2;
					state_w15	<= FILL256;
				end
				if (t==62) begin
					state_w15	<= READ;
				end
				if (t==63) begin
					state_mainOp<= UPDATE;
					state_w15	<= READ;
				end
				if (t==64) begin
					state_mainOp<= COMPUTE;	
				end
			end
			
			THIRD: begin
				if (t==13) begin
					state_w15	<= WTNEW;
				end
				if (t==62) begin
					state_mainOp<= UPDATE;
					state_w15	<= READ;
				end

				if (t==63) begin
					state_mainOp<= PREP;
					state_w15	<= READ;
				end
				if (t==64) begin
					state_mainOp<= COMPUTE;	
				end
			end
	endcase
	end
 
endmodule