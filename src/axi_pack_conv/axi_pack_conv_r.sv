//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "common_cells/registers.svh"

module axi_pack_conv_r #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I ,
  parameter int unsigned DataWidth_O ,
  parameter int unsigned FifoDepth ,
  parameter type         axi_ssr_r_chan_t    = logic,
  parameter type         axi_r_chan_t    = logic,
  parameter type         sarq_t   = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //sarq
  input sarq_t                          rsarq_i,
  input logic                           rsarq_push,
  output logic                          rsarq_full,
  //ssr axi r channel
  output axi_ssr_r_chan_t               ssr_r_chan_o,
  output logic                          ssr_r_valid_o,
  input logic                           ssr_r_ready_i,
  //std axi r channel
  input axi_r_chan_t                    r_chan_i,
  input logic                           r_valid_i,
  output logic                          r_ready_o
);

localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);

//******************************
//read standard axi request queue(RSARQ)
//******************************
  sarq_t rsarq_out;
  logic  rsarq_pop, rsarq_empty, _rsarq_push, _rsarq_full;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( FifoDepth  ),
    .dtype        ( sarq_t              )
  ) i_fifo_rsarq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (_rsarq_full),
    .empty_o (rsarq_empty),
    .usage_o (/* open */),
    .data_i (rsarq_i),
    .push_i (_rsarq_push),
    .data_o (rsarq_out),
    .pop_i (rsarq_pop)
  );

//*********
//Id isolation
//*********
always_comb begin
  rsarq_full = _rsarq_full;
  _rsarq_push = rsarq_push;
  if ( ~rsarq_empty & (rsarq_i.id != rsarq_out.id)) begin
    _rsarq_push = 0;
    rsarq_full = 1;
  end
end

//*********
//R Channel
//*********
  logic [DataAlign_I-1:0] r_input_offset;
  axi_pkg::len_t  r_input_cnt_d,r_input_cnt_q;
  `FFSR(r_input_cnt_q, r_input_cnt_d, '0, clk_i, rst_i)

  logic [DataAlign_O-1:0] r_output_offset;
  logic [DataAlign_O-1:0] r_output_cnt_d,r_output_cnt_q;
  `FFSR(r_output_cnt_q, r_output_cnt_d, '0, clk_i, rst_i)

  logic complete_current_read_beat, r_output_shack,r_input_shack;

  //Control signal
  assign r_input_offset = rsarq_out.ssr_offset+({3'b0,r_input_cnt_q}<<rsarq_out.ssr_size);
  assign r_output_offset = rsarq_out.std_offset+({3'b0,r_output_cnt_q}<<rsarq_out.ssr_size);

  assign complete_current_read_beat = (({1'b0,r_output_offset}+({3'b0,(rsarq_out.ssr_stride+1)}<<rsarq_out.ssr_size))>>DataAlign_O) | rsarq_out.same_size | (r_input_cnt_q==rsarq_out.ssr_len);
  assign r_output_shack = r_ready_o & r_valid_i;
  assign r_input_shack = ssr_r_ready_i & ssr_r_valid_o;
  assign r_input_cnt_d =  (rsarq_pop | rsarq_empty)? '0:
                          r_input_shack?              r_input_cnt_q+1:
                                                      r_input_cnt_q;
  assign r_output_cnt_d = (rsarq_pop | rsarq_empty)? '0:
                          r_input_shack?              r_output_cnt_q+rsarq_out.ssr_stride+1:
                                                      r_output_cnt_q;

  assign rsarq_pop = r_input_shack & (r_input_cnt_q==rsarq_out.ssr_len) & ~rsarq_empty;
  assign ssr_r_valid_o = r_valid_i & ~rsarq_empty;
  assign r_ready_o = r_input_shack & complete_current_read_beat;


  //Datapath
  assign    ssr_r_chan_o.data = ((r_chan_i.data>>{r_output_offset,3'b000})<<{r_input_offset,3'b000});
  assign    ssr_r_chan_o.last = (r_input_cnt_q==rsarq_out.ssr_len) & rsarq_out.ssr_last;
  assign    ssr_r_chan_o.resp = r_chan_i.resp;
  assign    ssr_r_chan_o.id   = r_chan_i.id;
  assign    ssr_r_chan_o.user = r_chan_i.user;//'0;


endmodule