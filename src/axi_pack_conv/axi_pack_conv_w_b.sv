//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "common_cells/registers.svh"

//Todo: fix Respond channel

module axi_pack_conv_w_b #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I ,
  parameter int unsigned DataWidth_O ,
  parameter int unsigned FifoDepth ,
  parameter type         axi_ssr_w_chan_t    = logic,
  parameter type         axi_w_chan_t    = logic,
  parameter type         axi_ssr_b_chan_t    = logic,
  parameter type         axi_b_chan_t    = logic,
  parameter type         sarq_t   = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //sarq
  input sarq_t                          wsarq_i,
  input logic                           wsarq_push,
  output logic                          wsarq_full,
  //ssr axi w channel
  input axi_ssr_w_chan_t                ssr_w_chan_i,
  input logic                           ssr_w_valid_i,
  output logic                          ssr_w_ready_o,
  //std axi w channel
  output axi_w_chan_t                   w_chan_o,
  output logic                          w_valid_o,
  input logic                           w_ready_i,
  //ssr axi b channel
  output axi_ssr_b_chan_t               ssr_b_chan_o,
  output logic                          ssr_b_valid_o,
  input logic                           ssr_b_ready_i,
  //std axi b channel
  input axi_b_chan_t                    b_chan_i,
  input logic                           b_valid_i,
  output logic                          b_ready_o
);

localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);

//******************************
//standard axi request queue(WSARQ)
//******************************
  sarq_t wsarq_out;
  logic  wsarq_pop, wsarq_empty;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( FifoDepth ),
    .dtype        ( sarq_t              )
  ) i_fifo_wsarq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),  
    .full_o (wsarq_full),
    .empty_o (wsarq_empty),
    .usage_o (/* open */),
    .data_i (wsarq_i),
    .push_i (wsarq_push),
    .data_o (wsarq_out),
    .pop_i (wsarq_pop)
  );

//*********
//W Channel
//*********

  logic [DataWidth_O/8-1:0] extened_w_strb;
  logic [DataWidth_O/8-1:0] w_strb_d,w_strb_q;
  `FFSR(w_strb_q, w_strb_d, '0, clk_i, rst_i)

  logic [DataWidth_I-1:0] w_input_data_mask;

  logic [DataWidth_O-1:0] extened_w_data;
  logic [DataWidth_O-1:0] w_data_d,w_data_q;
  `FFSR(w_data_q, w_data_d, '0, clk_i, rst_i)

  logic [DataAlign_I-1:0] w_input_offset;
  axi_pkg::len_t  w_input_cnt_d,w_input_cnt_q;
  `FFSR(w_input_cnt_q, w_input_cnt_d, '0, clk_i, rst_i)

  logic [DataAlign_O-1:0] w_output_offset;
  logic [DataAlign_O-1:0] w_output_cnt_d,w_output_cnt_q;
  `FFSR(w_output_cnt_q, w_output_cnt_d, '0, clk_i, rst_i)

  logic complete_current_write_beat, w_output_shack,w_input_shack;

  //Control signal
  assign w_input_offset = wsarq_out.ssr_offset+({3'b0,w_input_cnt_q}<<wsarq_out.ssr_size);
  assign w_output_offset = wsarq_out.std_offset+({3'b0,w_output_cnt_q}<<wsarq_out.ssr_size);

  assign complete_current_write_beat = (({1'b0,w_output_offset}+({3'b0,(wsarq_out.ssr_stride+1)}<<wsarq_out.ssr_size))>>DataAlign_O) | wsarq_out.same_size | (w_input_cnt_q==wsarq_out.ssr_len);
  assign w_output_shack = w_valid_o & w_ready_i;
  assign w_input_shack = ssr_w_valid_i & ssr_w_ready_o;
  assign w_input_cnt_d =  (wsarq_pop | wsarq_empty)? '0:
                          w_input_shack?              w_input_cnt_q+1:
                                                      w_input_cnt_q;
  assign w_output_cnt_d = (wsarq_pop | wsarq_empty)? '0:
                          w_input_shack?              w_output_cnt_q+wsarq_out.ssr_stride+1:
                                                      w_output_cnt_q;
  assign wsarq_pop = w_output_shack & (w_input_cnt_q==wsarq_out.ssr_len);
  assign w_valid_o = ssr_w_valid_i & ~wsarq_empty & complete_current_write_beat;
  assign ssr_w_ready_o = complete_current_write_beat? w_output_shack:ssr_w_valid_i;

  //Datapath
  genvar gi;
  for (gi = 0; gi < DataWidth_I; gi++) begin
    assign w_input_data_mask[gi] = ssr_w_chan_i.strb[gi/8];
  end
  assign w_strb_d = w_output_shack? '0:
                    w_input_shack?  w_chan_o.strb:
                                    w_strb_q;
  assign w_data_d = w_output_shack? '0:
                    w_input_shack?  w_chan_o.data:
                                    w_data_q;

  assign    extened_w_data = ssr_w_chan_i.data & w_input_data_mask;
  assign    w_chan_o.data = w_data_q | ((extened_w_data>>{w_input_offset,3'b000})<<{w_output_offset,3'b000});
  
  assign    extened_w_strb = ssr_w_chan_i.strb;
  assign    w_chan_o.strb = w_strb_q | ((extened_w_strb>>w_input_offset)<<w_output_offset);

  assign    w_chan_o.last = (w_input_cnt_q==wsarq_out.ssr_len);
  assign    w_chan_o.user = ssr_w_chan_i.user;


//*********
//B Channel
//*********
  logic ssr_b_valid_q,ssr_b_valid_d;
  `FFSR(ssr_b_valid_q, ssr_b_valid_d, '0, clk_i, rst_i)
  assign ssr_b_valid_d = (~ssr_b_valid_q & ssr_w_chan_i.last & w_input_shack)?  1:
                         (ssr_b_valid_q & ssr_b_ready_i)?                       0:
                                                                    ssr_b_valid_q;
  //Always set up b_valid, it may cause some errors, fix it ASAP
  assign ssr_b_valid_o = ssr_b_valid_q;
  assign ssr_b_chan_o.id = b_chan_i.id;//'0;
  assign ssr_b_chan_o.resp = axi_pkg::RESP_OKAY;
  assign ssr_b_chan_o.user = b_chan_i.user;
  //Come on ! Nobody cares about the ERROR! o(￣┰￣*)ゞ
  assign b_ready_o = 1;

endmodule 
