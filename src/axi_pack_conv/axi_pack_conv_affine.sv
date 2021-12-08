//Author: Chi Zhang <chizhang@student.ethz.ch>

`include "common_cells/registers.svh"
module axi_pack_conv_affine #(
  /// Number of outstanding loads.
  parameter int unsigned NumOutstandingRead ,
  /// Number of outstanding stores.
  parameter int unsigned NumOutstandingWrite ,
  /// Axi Width
  parameter int unsigned AddrWidth,
  parameter int unsigned AxiIdWidth,
  parameter int unsigned DataWidth_I,
  parameter int unsigned DataWidth_O ,
  /// Interface type
  parameter type         axi_ssr_req_t    = logic,
  parameter type         axi_ssr_rsp_t    = logic,
  parameter type         axi_req_t           = logic,
  parameter type         axi_rsp_t           = logic,
  ///Channel type
  parameter type         axi_ssr_aw_chan_t     = logic,
  parameter type         axi_ssr_ar_chan_t    = logic,
  parameter type         axi_ssr_w_chan_t    = logic,
  parameter type         axi_ssr_r_chan_t    = logic,
  parameter type         axi_ssr_b_chan_t    = logic,
  parameter type         axi_aw_chan_t    = logic,
  parameter type         axi_ar_chan_t    = logic,
  parameter type         axi_w_chan_t    = logic,
  parameter type         axi_r_chan_t    = logic,
  parameter type         axi_b_chan_t    = logic
)(
    input  logic           			clk_i,
  	input  logic           			rst_i,
  	//Axi Stream Interface Channel
    input  axi_ssr_req_t       	axi_ssr_req_i,
    output axi_ssr_rsp_t       	axi_ssr_rsp_o,
  	//Slave Interface channel
	  output axi_req_t 			 	axi_req_o,
  	input  axi_rsp_t 			 	axi_rsp_i
);
  localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
  localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);

  typedef struct packed {
    logic [AddrWidth-1:0]           addr;
    axi_pkg::size_t                 size;
    axi_pkg::len_t                  len;
    logic [AxiIdWidth-1:0]          id;
    axi_pack_pkg::ssr_user_t     user;
  } ssr_t;// ssr request

  typedef struct packed {
    logic [AxiIdWidth-1:0]          id;
    logic [DataAlign_I-1:0]         ssr_offset;
    axi_pkg::size_t                 ssr_size;
    axi_pack_pkg::stride_t       ssr_stride;
    logic [DataAlign_O-1:0]         std_offset;
    logic                           same_size;
    axi_pkg::len_t                  ssr_len;
    logic                           ssr_last;
  } sarq_t;

//***************
//AW & AR Channel
//*************** 
  ssr_t wrq_in,rrq_in;

  sarq_t wsarq_in, rsarq_in;
  logic  wsarq_push, rsarq_push, wsarq_full, rsarq_full;

  assign wrq_in = '{
    addr:       axi_ssr_req_i.aw.addr,
    size:       axi_ssr_req_i.aw.size,
    len:        axi_ssr_req_i.aw.len,
    id:         axi_ssr_req_i.aw.id,
    user:       axi_ssr_req_i.aw.user 
  }; 

  assign rrq_in = '{
    addr:       axi_ssr_req_i.ar.addr,
    size:       axi_ssr_req_i.ar.size,
    len:        axi_ssr_req_i.ar.len,
    id:         axi_ssr_req_i.ar.id,
    user:       axi_ssr_req_i.ar.user    
  };        

//******************************
//Standard Axi request generator
//******************************
//Generate Write Axi Request
axi_pack_conv_ax_affine #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .axi_ax_chan_t(axi_aw_chan_t),
  .ssr_t(ssr_t),
  .sarq_t(sarq_t)
  ) i_affine_to_std_aw(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //ssr
  .xrq_out(wrq_in),
  .xrq_empty(~axi_ssr_req_i.aw_valid),
  .xrq_pop(axi_ssr_rsp_o.aw_ready),
  //sarq
  .xsarq_in(wsarq_in),
  .xsarq_push(wsarq_push),
  .xsarq_full(wsarq_full),
  //std axi
  .ax_valid_o(axi_req_o.aw_valid),
  .ax_ready_i(axi_rsp_i.aw_ready),
  .ax_chan_o(axi_req_o.aw)
);


//Generate Read Axi Request
axi_pack_conv_ax_affine #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .axi_ax_chan_t(axi_ar_chan_t),
  .ssr_t(ssr_t),
  .sarq_t(sarq_t)
  ) i_affine_to_std_ar(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //ssr
  .xrq_out(rrq_in),
  .xrq_empty(~axi_ssr_req_i.ar_valid),
  .xrq_pop(axi_ssr_rsp_o.ar_ready),
  //sarq
  .xsarq_in(rsarq_in),
  .xsarq_push(rsarq_push),
  .xsarq_full(rsarq_full),
  //std axi
  .ax_valid_o(axi_req_o.ar_valid),
  .ax_ready_i(axi_rsp_i.ar_ready),
  .ax_chan_o(axi_req_o.ar)
);

axi_pack_conv_w_b #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .FifoDepth(NumOutstandingWrite),
  .axi_ssr_w_chan_t(axi_ssr_w_chan_t),
  .axi_w_chan_t(axi_w_chan_t),
  .axi_ssr_b_chan_t(axi_ssr_b_chan_t),
  .axi_b_chan_t(axi_b_chan_t),
  .sarq_t(sarq_t)
  )i_ssr_to_std_w_b(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //sarq
  .wsarq_i(wsarq_in),
  .wsarq_push(wsarq_push),
  .wsarq_full(wsarq_full),
  //ssr axi w channel
  .ssr_w_chan_i(axi_ssr_req_i.w),
  .ssr_w_valid_i(axi_ssr_req_i.w_valid),
  .ssr_w_ready_o(axi_ssr_rsp_o.w_ready),
  //std axi w channel
  .w_chan_o(axi_req_o.w),
  .w_valid_o(axi_req_o.w_valid),
  .w_ready_i(axi_rsp_i.w_ready),
  //ssr axi b channel
  .ssr_b_chan_o(axi_ssr_rsp_o.b),
  .ssr_b_valid_o(axi_ssr_rsp_o.b_valid),
  .ssr_b_ready_i(axi_ssr_req_i.b_ready),
  //std axi b channel
  .b_chan_i(axi_rsp_i.b),
  .b_valid_i(axi_rsp_i.b_valid),
  .b_ready_o(axi_req_o.b_ready)
);

axi_pack_conv_r #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .FifoDepth(NumOutstandingRead),
  .axi_ssr_r_chan_t(axi_ssr_r_chan_t),
  .axi_r_chan_t(axi_r_chan_t),
  .sarq_t(sarq_t)
  )i_ssr_from_std_r(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //sarq
  .rsarq_i(rsarq_in),
  .rsarq_push(rsarq_push),
  .rsarq_full(rsarq_full),
  //ssr axi r channel
  .ssr_r_chan_o(axi_ssr_rsp_o.r),
  .ssr_r_valid_o(axi_ssr_rsp_o.r_valid),
  .ssr_r_ready_i(axi_ssr_req_i.r_ready),
  //std axi r channel
  .r_chan_i(axi_rsp_i.r),
  .r_valid_i(axi_rsp_i.r_valid),
  .r_ready_o(axi_req_o.r_ready)
);


endmodule 



