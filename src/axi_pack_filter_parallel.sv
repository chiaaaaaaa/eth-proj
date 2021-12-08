//Author: Chi Zhang <chizhang@student.ethz.ch>

//Filter for SSR AXI (version1: use affine and direct filter wrapped by AXI mux/demux )
//1. Only support INC BURST
//2. Do not support atomic transaction
//3. Always trust last signal for Write and Read.
//4. Input and Output AddrWidth equal to each other
//5. Do not support Read Transaction with different IDs
//6. The Output DataWidth must >= Input DataWidth 
//7. Do not support misalignment transfer


`include "common_cells/registers.svh"
module axi_pack_filter_parallel #(
  /// Number of outstanding loads.
  parameter int unsigned NumOutstandingRead = 1,
  /// Number of outstanding stores.
  parameter int unsigned NumOutstandingWrite = 1,
  /// Indirect configuration
  parameter int unsigned NumIndexBeat = 8,
  parameter int unsigned NumIndexBeatMargin = 4,
  parameter int unsigned IndexFetchQueueDeepth = 5 ,
  /// Axi Width
  parameter int unsigned AddrWidth = 32,
  parameter int unsigned AxiIdWidth_I,
  parameter int unsigned AxiIdWidth_O,
  parameter int unsigned DataWidth_I = 32,
  parameter int unsigned DataWidth_O = 32,
  /// Interface type
  parameter type        axi_ssr_req_t    	= logic,
  parameter type        axi_ssr_rsp_t    	= logic,
  parameter type        axi_req_t           = logic,
  parameter type        axi_rsp_t           = logic,
  // Channel type
  parameter type        axi_aw_chan_t    	= logic,
  parameter type 		axi_w_chan_t   		= logic,
  parameter type 		axi_b_chan_t   		= logic,
  parameter type 		axi_ar_chan_t   	= logic,
  parameter type 		axi_r_chan_t   		= logic,
  parameter type 		axi_ssr_aw_chan_t   = logic,
  parameter type 		axi_ssr_w_chan_t    = logic,
  parameter type 		axi_ssr_b_chan_t    = logic,
  parameter type 		axi_ssr_ar_chan_t   = logic,
  parameter type 		axi_ssr_r_chan_t    = logic
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

  logic ssr_aw_select,ssr_ar_select; 
  axi_pack_pkg::ssr_user_t aw_user,ar_user;
  assign aw_user = axi_ssr_req_i.aw.user;
  assign ar_user = axi_ssr_req_i.ar.user;
  assign ssr_aw_select = aw_user.indirect_enable;
  assign ssr_ar_select = ar_user.indirect_enable;

  axi_ssr_req_t [1:0] mst_reqs;
  axi_ssr_rsp_t [1:0] mst_rsps;
  axi_ssr_req_t	[1:0] slv_reqs;
  axi_ssr_rsp_t	[1:0] slv_rsps;


axi_demux #(
      .AtopSupport    ( 0            ),
      .AxiIdWidth     ( AxiIdWidth_I ),  // ID Width
      .aw_chan_t      ( axi_ssr_aw_chan_t          ),  // AW Channel Type
      .w_chan_t       ( axi_ssr_w_chan_t               ),  //  W Channel Type
      .b_chan_t       ( axi_ssr_b_chan_t           ),  //  B Channel Type
      .ar_chan_t      ( axi_ssr_ar_chan_t          ),  // AR Channel Type
      .r_chan_t       ( axi_ssr_r_chan_t           ),  //  R Channel Type
      .req_t          ( axi_ssr_req_t              ),
      .resp_t         ( axi_ssr_rsp_t             ),
      .NoMstPorts     ( 2     )
    ) i_ssr_axi_demux (
      .clk_i(clk_i),   // Clock
      .rst_ni(~rst_i),  // Asynchronous reset active low
      .test_i('0),  // Testmode enable
      .slv_req_i       ( axi_ssr_req_i  ),
      .slv_aw_select_i ( ssr_aw_select       ),
      .slv_ar_select_i ( ssr_ar_select       ),
      .slv_resp_o      ( axi_ssr_rsp_o ),
      .mst_reqs_o      ( mst_reqs         ),
      .mst_resps_i     ( mst_rsps        )
    );

/*affine*/
axi_pack_conv_affine #(
    .NumOutstandingRead (NumOutstandingRead),
    .NumOutstandingWrite(NumOutstandingWrite),
    .AddrWidth(AddrWidth),
    .DataWidth_I(DataWidth_I),
    .DataWidth_O(DataWidth_O),
    .AxiIdWidth(AxiIdWidth_I),
    .axi_ssr_req_t      (axi_ssr_req_t),
    .axi_ssr_rsp_t      (axi_ssr_rsp_t),
    .axi_req_t (axi_ssr_req_t),
    .axi_rsp_t (axi_ssr_rsp_t),
    .axi_ssr_aw_chan_t(axi_ssr_aw_chan_t),
    .axi_ssr_ar_chan_t(axi_ssr_ar_chan_t),
    .axi_ssr_w_chan_t(axi_ssr_w_chan_t),
    .axi_ssr_r_chan_t(axi_ssr_r_chan_t),
    .axi_ssr_b_chan_t(axi_ssr_b_chan_t),
    .axi_aw_chan_t(axi_ssr_aw_chan_t),
    .axi_ar_chan_t(axi_ssr_ar_chan_t),
    .axi_w_chan_t(axi_ssr_w_chan_t),
    .axi_r_chan_t(axi_ssr_r_chan_t),
    .axi_b_chan_t(axi_ssr_b_chan_t)
)i_filter_affine(
    .clk_i (clk_i),
    .rst_i (rst_i),
    .axi_ssr_req_i(mst_reqs[0]),
    .axi_ssr_rsp_o(mst_rsps[0]),
    .axi_req_o    (slv_reqs[0]),
    .axi_rsp_i    (slv_rsps[0])
);

/*indirect*/
axi_pack_conv_indirect #(
    .NumOutstandingRead (NumOutstandingRead),
    .NumOutstandingWrite(NumOutstandingWrite),
    .NumIndexBeat(NumIndexBeat),
    .NumIndexBeatMargin(NumIndexBeatMargin),
    .IndexFetchQueueDeepth(IndexFetchQueueDeepth),
    .AddrWidth(AddrWidth),
    .DataWidth_I(DataWidth_I),
    .DataWidth_O(DataWidth_O),
    .AxiIdWidth(AxiIdWidth_I),
    .axi_ssr_req_t      (axi_ssr_req_t),
    .axi_ssr_rsp_t      (axi_ssr_rsp_t),
    .axi_req_t (axi_ssr_req_t),
    .axi_rsp_t (axi_ssr_rsp_t),
    .axi_ssr_aw_chan_t(axi_ssr_aw_chan_t),
    .axi_ssr_ar_chan_t(axi_ssr_ar_chan_t),
    .axi_ssr_w_chan_t(axi_ssr_w_chan_t),
    .axi_ssr_r_chan_t(axi_ssr_r_chan_t),
    .axi_ssr_b_chan_t(axi_ssr_b_chan_t),
    .axi_aw_chan_t(axi_ssr_aw_chan_t),
    .axi_ar_chan_t(axi_ssr_ar_chan_t),
    .axi_w_chan_t(axi_ssr_w_chan_t),
    .axi_r_chan_t(axi_ssr_r_chan_t),
    .axi_b_chan_t(axi_ssr_b_chan_t)
)i_filter_indirect(
    .clk_i (clk_i),
    .rst_i (rst_i),
    .axi_ssr_req_i(mst_reqs[1]),
    .axi_ssr_rsp_o(mst_rsps[1]),
    .axi_req_o    (slv_reqs[1]),
    .axi_rsp_i    (slv_rsps[1])
);

axi_mux #(
      .SlvAxiIDWidth ( AxiIdWidth_I ), // ID width of the slave ports
      .slv_aw_chan_t ( axi_ssr_aw_chan_t          ), // AW Channel Type, slave ports
      .mst_aw_chan_t ( axi_aw_chan_t          ), // AW Channel Type, master port
      .w_chan_t      ( axi_w_chan_t               ), //  W Channel Type, all ports
      .slv_b_chan_t  ( axi_ssr_b_chan_t           ), //  B Channel Type, slave ports
      .mst_b_chan_t  ( axi_b_chan_t           ), //  B Channel Type, master port
      .slv_ar_chan_t ( axi_ssr_ar_chan_t          ), // AR Channel Type, slave ports
      .mst_ar_chan_t ( axi_ar_chan_t          ), // AR Channel Type, master port
      .slv_r_chan_t  ( axi_ssr_r_chan_t           ), //  R Channel Type, slave ports
      .mst_r_chan_t  ( axi_r_chan_t           ), //  R Channel Type, master port
      .slv_req_t     ( axi_ssr_req_t              ),
      .slv_resp_t    ( axi_ssr_rsp_t             ),
      .mst_req_t     ( axi_req_t              ),
      .mst_resp_t    ( axi_rsp_t             ),
      .NoSlvPorts    ( 2         ) 
    ) i_axi_mux (
      .clk_i(clk_i),   // Clock
      .rst_ni(~rst_i),  // Asynchronous reset active low
      .test_i('0),  // Test Mode enable
      .slv_reqs_i  ( slv_reqs         ),
      .slv_resps_o ( slv_rsps        ),
      .mst_req_o   ( axi_req_o  ),
      .mst_resp_i  ( axi_rsp_i )
    );



endmodule