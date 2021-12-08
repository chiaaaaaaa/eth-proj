//Author: Chi Zhang <chizhang@student.ethz.ch>

package axi_pack_filter_parallel_wrap_pkg;

  `include "axi/assign.svh"
  `include "axi/typedef.svh"
  import axi_pack_pkg::*;
  
  //Set up snitch lsu parameters
  localparam int unsigned AddrWidth = 32;
  localparam int unsigned DataWidth = 32;
  localparam int unsigned DataAlign = $clog2(DataWidth/8);
  localparam int unsigned AxiUserWidth = $bits(ssr_user_t);
  localparam int unsigned AxiIdWidth = 8;
  localparam int unsigned AxiExtIdWidth = 9;
  localparam int unsigned AxiStrbWidth = DataWidth / 8;
  localparam int unsigned NumIntOutstandingStores = 8;
  localparam int unsigned NumIntOutstandingLoads = 8;
  localparam bit AxiMemWarnUninitialized = 1'b0;

  //define types
  typedef logic[4:0] tag_t;
  typedef logic[AddrWidth-1:0] addr_t;
  typedef logic[DataWidth-1:0] data_t;

  typedef logic [AddrWidth-1:0] axi_addr_t;
  typedef logic [DataWidth-1:0] axi_data_t;
  typedef logic [AxiIdWidth-1:0]   axi_id_t;
  typedef logic [AxiExtIdWidth-1:0]   axi_idext_id_t;
  typedef logic [AxiStrbWidth-1:0] axi_strb_t;
  typedef logic [AxiUserWidth-1:0] axi_user_t;

  `AXI_TYPEDEF_AW_CHAN_T(axi_aw_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_W_CHAN_T(axi_w_t, axi_data_t, axi_strb_t, axi_user_t)
  `AXI_TYPEDEF_B_CHAN_T(axi_b_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_AR_CHAN_T(axi_ar_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_R_CHAN_T(axi_r_t, axi_data_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_REQ_T(axi_req_t, axi_aw_t, axi_w_t, axi_ar_t)
  `AXI_TYPEDEF_RESP_T(axi_rsp_t, axi_b_t, axi_r_t)

  `AXI_TYPEDEF_AW_CHAN_T(axi_idext_aw_t, axi_addr_t, axi_idext_id_t, axi_user_t)
  `AXI_TYPEDEF_W_CHAN_T(axi_idext_w_t, axi_data_t, axi_strb_t, axi_user_t)
  `AXI_TYPEDEF_B_CHAN_T(axi_idext_b_t, axi_idext_id_t, axi_user_t)
  `AXI_TYPEDEF_AR_CHAN_T(axi_idext_ar_t, axi_addr_t, axi_idext_id_t, axi_user_t)
  `AXI_TYPEDEF_R_CHAN_T(axi_idext_r_t, axi_data_t, axi_idext_id_t, axi_user_t)
  `AXI_TYPEDEF_REQ_T(axi_idext_req_t, axi_idext_aw_t, axi_idext_w_t, axi_idext_ar_t)
  `AXI_TYPEDEF_RESP_T(axi_idext_rsp_t, axi_idext_b_t, axi_idext_r_t)

endpackage


module axi_pack_filter_parallel_wrap
  import axi_pack_filter_parallel_wrap_pkg::*;
#(
  /// Number of outstanding loads.
  parameter int unsigned NumOutstandingRead = 1,
  /// Number of outstanding stores.
  parameter int unsigned NumOutstandingWrite = 1
) (
  input  logic            clk_i,
  input  logic            rst_i,
  //Axi Stream Interface Channel
  input  axi_req_t    axi_ssr_req_i,
  output axi_rsp_t    axi_ssr_rsp_o,
  //Slave Interface channel
  output axi_idext_req_t axi_req_o,
  input  axi_idext_rsp_t axi_rsp_i
);

axi_pack_filter_parallel #(
  .NumOutstandingRead (NumOutstandingRead),
  .NumOutstandingWrite(NumOutstandingWrite),
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth),
  .DataWidth_O(DataWidth),
  .AxiIdWidth_I(AxiIdWidth),
  .AxiIdWidth_O(AxiExtIdWidth),
  .axi_ssr_req_t       ( axi_req_t ),
  .axi_ssr_rsp_t       ( axi_rsp_t ),
  .axi_req_t           ( axi_idext_req_t ),
  .axi_rsp_t           ( axi_idext_rsp_t ),
  .axi_aw_chan_t       ( axi_idext_aw_t   ),
  .axi_w_chan_t        ( axi_idext_w_t    ),
  .axi_b_chan_t        ( axi_idext_b_t    ),
  .axi_ar_chan_t       ( axi_idext_ar_t   ),
  .axi_r_chan_t        ( axi_idext_r_t    ),
  .axi_ssr_aw_chan_t   ( axi_aw_t  ),
  .axi_ssr_w_chan_t    ( axi_w_t   ),
  .axi_ssr_b_chan_t    ( axi_b_t   ),
  .axi_ssr_ar_chan_t   ( axi_ar_t  ),
  .axi_ssr_r_chan_t    ( axi_r_t   )
) i_axi_ssr_filter_v1 (
  .clk_i,
  .rst_i,
  .axi_ssr_req_i,
  .axi_ssr_rsp_o,
  .axi_req_o,
  .axi_rsp_i
);

endmodule