//Author: Chi Zhang <chizhang@student.ethz.ch>

//Load Store Unit for Stream Senantic Register (SSR) AXI
//1. Do not support atomic transaction
//2. Do not support misalignment transfer
//3. No Buffered Write
//4. AXI DataWidth must equal to LSU DataWidth
//5. Always trust last signal for load and store.
//6. Support Affine Burst.
//7. AXI UserWidth must equal to LSU q_ssr_user width. 
`include "common_cells/registers.svh"
module snitch_lsu_ssr #(
  /// Number of outstanding loads.
  parameter int unsigned NumOutstandingLoads = 1,
  /// Number of outstanding stores.
  parameter int unsigned NumOutstandingStores = 1,
  /// Whether to NaN Box values. Used for floating-point load/stores.
  parameter bit          WAIT_WRITE_RESPOND = 1'b0, // fifo is in fall-through mode
  parameter bit          NaNBox              = 0,
  parameter type         lsu_req_t           = logic,
  parameter type         lsu_rsp_t           = logic,
  parameter type         axi_req_t           = logic,
  parameter type         axi_rsp_t           = logic,
  parameter int          ID 				         = 0
) (
	  input  logic           clk_i,
  	input  logic           rst_i,
  	//LSU Interface channel
    input  lsu_req_t       lsu_req_i,
    output lsu_rsp_t       lsu_rsp_o,
  	//Memory Interface channel
	  output axi_req_t 			 axi_req_o,
  	input  axi_rsp_t 			 axi_rsp_i
);
  localparam int unsigned AddrWidth = $size(axi_req_o.aw.addr);
  localparam int unsigned DataWidth = $size(axi_req_o.w.data);
  localparam int unsigned AxiIdWidth = $size(axi_req_o.aw.id);
  localparam int unsigned DataAlign = $clog2(DataWidth/8);
  localparam int unsigned AxiBurstLengthWidth = $size(axi_req_o.aw.len);
  localparam int unsigned AxiUserWidth = $size(axi_req_o.aw.user);

  logic [63:0]          ld_result;
  logic [63:0]          shifted_data;
  logic [DataAlign-1:0] write_offset;
  logic [DataAlign-1:0] read_offset;

  logic w_ready_d,w_ready_q;
  `FFSR(w_ready_q, w_ready_d, '0, clk_i, rst_i)

  logic [DataAlign-1:0] write_cnt_d,write_cnt_q,read_cnt_d,read_cnt_q;
  `FFSR(write_cnt_q, write_cnt_d, '0, clk_i, rst_i)
  `FFSR(read_cnt_q, read_cnt_d, '0, clk_i, rst_i)

//************************
//Store Address Queue (SAQ)
//************************
  typedef struct packed {
    logic [DataAlign-1:0]         offset;
    logic [1:0]                   size;
  } saq_t;

  saq_t saq_in, saq_out;
  logic saq_push, saq_pop, saq_full, saq_empty;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( NumOutstandingStores ),
    .dtype        ( saq_t               )
  ) i_fifo_saq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (saq_full),
    .empty_o (saq_empty),
    .usage_o (/* open */),
    .data_i (saq_in),
    .push_i (saq_push),
    .data_o (saq_out),
    .pop_i (saq_pop)
  );

  assign saq_in = '{
    offset:       lsu_req_i.q.q_addr[DataAlign-1:0],
    size:         lsu_req_i.q.q_size
  };        


//************************
//Load Address Queue (LAQ)
//************************
  typedef struct packed {
    logic                         sign_ext;
    logic [DataAlign-1:0]         offset;
    logic [1:0]                   size;
  } laq_t;

  laq_t laq_in, laq_out;
  logic laq_push, laq_pop, laq_full, laq_empty;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( NumOutstandingLoads ),
    .dtype        ( laq_t               )
  ) i_fifo_laq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (laq_full),
    .empty_o (laq_empty),
    .usage_o (/* open */),
    .data_i (laq_in),
    .push_i (laq_push),
    .data_o (laq_out),
    .pop_i (laq_pop)
  );

  assign laq_in = '{
    sign_ext:     lsu_req_i.q.q_signed,
    offset:       lsu_req_i.q.q_addr[DataAlign-1:0],
    size:         lsu_req_i.q.q_size
  };        

//*********
//Data Path
//*********

//AW Channel
  assign    axi_req_o.aw.addr   = lsu_req_i.q.q_addr;
  assign    axi_req_o.aw.burst  = axi_pkg::BURST_INCR;
  assign    axi_req_o.aw.lock   = '0;//Never use atomic transaction
  assign    axi_req_o.aw.cache  = axi_pkg::CACHE_MODIFIABLE;
if (ID<0) begin
  logic [AxiIdWidth-1:0] w_id_q,w_id_d;
  `FFSR(w_id_q, w_id_d, '0, clk_i, rst_i)
  assign    w_id_d = (axi_req_o.aw_valid & axi_rsp_i.aw_ready)? w_id_q+1:w_id_q;
  assign    axi_req_o.aw.id     = w_id_q;
end else begin
  assign    axi_req_o.aw.id     = $unsigned(ID);
end
  assign    axi_req_o.aw.len    = lsu_req_i.q.q_len;
  assign    axi_req_o.aw.size   = lsu_req_i.q.q_size;
  assign    axi_req_o.aw.prot   = '0;
  assign    axi_req_o.aw.qos    = '0;
  assign    axi_req_o.aw.region = '0;
  assign    axi_req_o.aw.atop   = '0;
  assign    axi_req_o.aw.user   = lsu_req_i.q.q_ssr_user[AxiUserWidth-1:0];

//W Channel
  assign    write_offset = saq_out.offset+({3'b0,write_cnt_q}<<saq_out.size);
  assign    axi_req_o.w.data = lsu_req_i.s.s_data<<{write_offset, 3'b000};
  assign    axi_req_o.w.last = lsu_req_i.s.s_last;
  assign    axi_req_o.w.user = '0;
  // Generate byte enable mask.
  always_comb begin
    unique case (saq_out.size)
      2'b00: axi_req_o.w.strb = ('b1 << write_offset[DataAlign-1:0]);
      2'b01: axi_req_o.w.strb = ('b11 << write_offset[DataAlign-1:0]);
      2'b10: axi_req_o.w.strb = ('b1111 << write_offset[DataAlign-1:0]);
      2'b11: axi_req_o.w.strb = '1;
      default: axi_req_o.w.strb = '0;
    endcase
  end
 
//AR Channel
if (ID<0) begin
  logic [AxiIdWidth-1:0] r_id_q,r_id_d;
  `FFSR(r_id_q, r_id_d, '0, clk_i, rst_i)
  assign    r_id_d = (axi_req_o.ar_valid & axi_rsp_i.ar_ready)? r_id_q+1:r_id_q;
  assign    axi_req_o.ar.id     = r_id_q;
end else begin
  assign    axi_req_o.ar.id     = $unsigned(ID);
end
  assign    axi_req_o.ar.addr   = lsu_req_i.q.q_addr;
  assign    axi_req_o.ar.burst  = axi_pkg::BURST_INCR;
  assign    axi_req_o.ar.lock   = '0;//Never use atomic transaction
  assign    axi_req_o.ar.cache  = axi_pkg::CACHE_MODIFIABLE; 
  assign    axi_req_o.ar.size   = lsu_req_i.q.q_size;
  assign    axi_req_o.ar.len    = lsu_req_i.q.q_len;
  assign    axi_req_o.ar.prot   = '0;
  assign    axi_req_o.ar.qos    = '0;
  assign    axi_req_o.ar.region = '0;
  assign    axi_req_o.ar.user   = lsu_req_i.q.q_ssr_user[AxiUserWidth-1:0];

//Reture Path
  assign    read_offset = laq_out.offset+({3'b0,read_cnt_q}<<laq_out.size);
  assign    shifted_data = axi_rsp_i.r.data >> {read_offset, 3'b000};
  always_comb begin
    unique case (laq_out.size)
      2'b00: ld_result = {{56{(shifted_data[7] | NaNBox) & laq_out.sign_ext}}, shifted_data[7:0]};
      2'b01: ld_result = {{48{(shifted_data[15] | NaNBox) & laq_out.sign_ext}}, shifted_data[15:0]};
      2'b10: ld_result = {{32{(shifted_data[31] | NaNBox) & laq_out.sign_ext}}, shifted_data[31:0]};
      2'b11: ld_result = shifted_data;
      default: ld_result = shifted_data;
    endcase
  end

  assign lsu_rsp_o.l.l_data = ld_result[DataWidth-1:0];
  assign lsu_rsp_o.l.l_error = axi_rsp_i.r.resp[1];
  assign lsu_rsp_o.l.l_last = axi_rsp_i.r.last;

//**************
//Control Signal
//**************  

//Stream Pattern
  logic push_done_q, push_done_d;
  `FFSR(push_done_q, push_done_d, '0, clk_i, rst_i)

  assign axi_req_o.ar_valid = lsu_req_i.q_valid & ~lsu_req_i.q.q_write & (~laq_full | push_done_q);
  assign axi_req_o.aw_valid = lsu_req_i.q_valid & lsu_req_i.q.q_write & (~saq_full | push_done_q) ;

  assign lsu_rsp_o.q_ready  = (axi_req_o.ar_valid & axi_rsp_i.ar_ready) | 
                              (axi_req_o.aw_valid & axi_rsp_i.aw_ready);

  assign push_done_d = lsu_rsp_o.q_ready? 0: (saq_push | laq_push)? 1: push_done_q;
  assign saq_push = ~push_done_q & ~saq_full & lsu_req_i.q.q_write & axi_req_o.aw_valid;
  assign laq_push = ~push_done_q & ~laq_full & ~lsu_req_i.q.q_write & axi_req_o.ar_valid;

  // assign saq_push = lsu_rsp_o.q_ready & lsu_req_i.q.q_write;
  // assign laq_push = lsu_rsp_o.q_ready & ~lsu_req_i.q.q_write;

//Write Stream
  always_comb begin
    if (WAIT_WRITE_RESPOND) begin
      axi_req_o.w_valid = ~saq_empty & ~w_ready_q & lsu_req_i.s_valid;
      saq_pop = ~saq_empty & w_ready_q & axi_req_o.b_ready & axi_rsp_i.b_valid;
    end else begin
      axi_req_o.w_valid = ~saq_empty & lsu_req_i.s_valid ;
      saq_pop = ~saq_empty & lsu_req_i.s.s_last& lsu_req_i.s_valid & lsu_rsp_o.s_ready;
    end
  end

  assign lsu_rsp_o.s_ready = axi_req_o.w_valid & axi_rsp_i.w_ready;
  assign w_ready_d =  (saq_pop | saq_empty)?                                    1'b0 :
                      (lsu_req_i.s.s_last& lsu_req_i.s_valid & lsu_rsp_o.s_ready)?    1'b1 :
                                                                                w_ready_q;
  assign write_cnt_d =  (saq_pop | saq_empty)?                    1'b0:
                        (axi_req_o.w_valid & axi_rsp_i.w_ready)?  write_cnt_q+1:
                                                                  write_cnt_q;

//Wtire response
  //Come on ! Nobody cares about the ERROR! o(￣┰￣*)ゞ
  assign axi_req_o.b_ready = 1;

//Read Stream
  assign lsu_rsp_o.l_valid =  axi_rsp_i.r_valid & ~laq_empty;
  assign axi_req_o.r_ready = lsu_rsp_o.l_valid & lsu_req_i.l_ready;
  assign read_cnt_d = (laq_pop | laq_empty)?  1'b0:
                      axi_req_o.r_ready?      read_cnt_q + 1:
                                              read_cnt_q;
  assign laq_pop = axi_req_o.r_ready & axi_rsp_i.r_valid & axi_rsp_i.r.last;

endmodule 