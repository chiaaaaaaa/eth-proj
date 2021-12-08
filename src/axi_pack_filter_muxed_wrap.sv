//Author: Chi Zhang <chizhang@student.ethz.ch>

package axi_pack_filter_muxed_wrap_pkg;

 `include "axi/assign.svh"
  `include "axi/typedef.svh"
  import axi_pack_pkg::*;

  localparam int unsigned NumOutstandingRead = 1;
  localparam int unsigned NumOutstandingwrite = 1;
  localparam int unsigned NumInFlightWrite = 0;
  localparam int unsigned NumInFlightRead = 0;
  localparam int unsigned NumIndexBeat = 1;
  localparam int unsigned NumIndexBeatMargin = 0;
  localparam int unsigned IndexFetchQueueDeepth = 0;

  localparam int unsigned AddrWidth = 32;
  //input axi
  localparam int unsigned DataWidth_I = 32;
  localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
  localparam int unsigned AxiUserWidth_I = $bits(axi_pack_pkg::ssr_user_t);
  localparam int unsigned AxiIdWidth_I = 2;
  localparam int unsigned AxiStrbWidth_I = DataWidth_I / 8;
  //output axi
  localparam int unsigned DataWidth_O = 32;
  localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);
  localparam int unsigned AxiUserWidth_O = 1;
  localparam int unsigned AxiIdWidth_O = 2;
  localparam int unsigned AxiStrbWidth_O = DataWidth_O / 8;

  typedef logic [AddrWidth-1:0] axi_addr_t;

  typedef logic [DataWidth_I-1:0] axi_ssr_data_t;
  typedef logic [AxiIdWidth_I-1:0]   axi_ssr_id_t;
  typedef logic [AxiStrbWidth_I-1:0] axi_ssr_strb_t;
  typedef logic [AxiUserWidth_I-1:0] axi_ssr_user_t;
  typedef logic [DataWidth_O-1:0] axi_data_t;
  typedef logic [AxiIdWidth_O-1:0]   axi_id_t;
  typedef logic [AxiStrbWidth_O-1:0] axi_strb_t;
  typedef logic [AxiUserWidth_O-1:0] axi_user_t;

  `AXI_TYPEDEF_AW_CHAN_T(axi_ssr_aw_t, axi_addr_t, axi_ssr_id_t, axi_ssr_user_t)
  `AXI_TYPEDEF_W_CHAN_T(axi_ssr_w_t, axi_ssr_data_t, axi_ssr_strb_t, axi_user_t)
  `AXI_TYPEDEF_B_CHAN_T(axi_ssr_b_t, axi_ssr_id_t, axi_user_t)
  `AXI_TYPEDEF_AR_CHAN_T(axi_ssr_ar_t, axi_addr_t, axi_ssr_id_t, axi_ssr_user_t)
  `AXI_TYPEDEF_R_CHAN_T(axi_ssr_r_t, axi_ssr_data_t, axi_ssr_id_t, axi_user_t)
  `AXI_TYPEDEF_REQ_T(axi_ssr_req_t, axi_ssr_aw_t, axi_ssr_w_t, axi_ssr_ar_t)
  `AXI_TYPEDEF_RESP_T(axi_ssr_rsp_t, axi_ssr_b_t, axi_ssr_r_t)

  `AXI_TYPEDEF_AW_CHAN_T(axi_aw_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_W_CHAN_T(axi_w_t, axi_data_t, axi_strb_t, axi_user_t)
  `AXI_TYPEDEF_B_CHAN_T(axi_b_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_AR_CHAN_T(axi_ar_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_R_CHAN_T(axi_r_t, axi_data_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_REQ_T(axi_req_t, axi_aw_t, axi_w_t, axi_ar_t)
  `AXI_TYPEDEF_RESP_T(axi_rsp_t, axi_b_t, axi_r_t)

endpackage

module axi_pack_filter_muxed_wrap
	import axi_pack_filter_muxed_wrap_pkg::*;
(
    input  logic           			clk_i,
  	input  logic           			rst_i,
  	//Axi Stream Interface Channel
    input  axi_ssr_req_t       	axi_ssr_req_i,
    output axi_ssr_rsp_t       	axi_ssr_rsp_o,
  	//Slave Interface channel
		output axi_req_t 			 			axi_req_o,
  	input  axi_rsp_t 			 			axi_rsp_i
);

  axi_pack_filter_muxed #(
    .NumOutstandingRead (NumOutstandingRead),
    .NumOutstandingWrite(NumOutstandingWrite),
    /// Spill register 
    .NumOutstandingwrite(NumOutstandingwrite),
    .NumInFlightRead(NumInFlightRead),
    /// Indirect configuration
    .NumIndexBeat(NumIndexBeat),
    .NumIndexBeatMargin(NumIndexBeatMargin),
    .IndexFetchQueueDeepth(IndexFetchQueueDeepth),
    //Axi Configuration
    .AddrWidth(AddrWidth),
    .DataWidth_I(DataWidth_I),
    .DataWidth_O(DataWidth_O),
    .AxiIdWidth(AxiIdWidth_I),
    //Type definition
    .axi_ssr_req_t      (axi_ssr_req_t),
    .axi_ssr_rsp_t      (axi_ssr_rsp_t),
    .axi_req_t (axi_req_t),
    .axi_rsp_t (axi_rsp_t),
    .axi_ssr_aw_chan_t(axi_ssr_aw_t),
    .axi_ssr_ar_chan_t(axi_ssr_ar_t),
    .axi_ssr_w_chan_t(axi_ssr_w_t),
    .axi_ssr_r_chan_t(axi_ssr_r_t),
    .axi_ssr_b_chan_t(axi_ssr_b_t),
    .axi_aw_chan_t(axi_aw_t),
    .axi_ar_chan_t(axi_ar_t),
    .axi_w_chan_t(axi_w_t),
    .axi_r_chan_t(axi_r_t),
    .axi_b_chan_t(axi_b_t)
)i_axi_pack_filter_muxed(
    .clk_i,
    .rst_i,
    .axi_ssr_req_i,
    .axi_ssr_rsp_o,
    .axi_req_o,
    .axi_rsp_i
);

endmodule

