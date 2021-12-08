//Author: Chi Zhang <chizhang@student.ethz.ch>

//Filter for SSR AXI (version2: merge indirect and affine part to have better performance and low area) 
//1. Only support INC BURST
//2. Do not support atomic transaction
//3. Always trust last signal for Write and Read.
//4. Input and Output AddrWidth equal to each other
//5. Do not support Read Transaction with different IDs,
//6. The Output DataWidth must >= Input DataWidth 
//7. Do not support misalignment transfer
//8. Rule for index Id: E.g: IdWidth=5, 11110->Read index 11111->Write index

`include "common_cells/registers.svh"
module axi_pack_filter_muxed #(
	/// Number of outstanding loads.
  parameter int unsigned NumOutstandingRead = 1,
  /// Number of outstanding stores.
  parameter int unsigned NumOutstandingWrite = 1,
  /// Spill register 
  parameter int unsigned NumInFlightWrite = 0,
  parameter int unsigned NumInFlightRead = 0,
  /// Indirect configuration
  parameter int unsigned NumIndexBeat = 1,
  parameter int unsigned NumIndexBeatMargin = 0,
  parameter int unsigned IndexFetchQueueDeepth = 0 ,
  /// Axi Width
  parameter int unsigned AddrWidth = 32,
  parameter int unsigned AxiIdWidth,
  parameter int unsigned DataWidth_I = 32,
  parameter int unsigned DataWidth_O = 32,
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
  parameter type         axi_aw_chan_t 	  = logic,
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

  localparam int unsigned AxiIdWidthSub = AxiIdWidth-1;
  // logic [AxiIdWidth-1:0]   read_index_id,write_index_id;
  localparam logic [AxiIdWidth-1:0] ReadIndexId = {{AxiIdWidthSub{1'b1}},1'b0};
  localparam logic [AxiIdWidth-1:0] WriteIndexId = {AxiIdWidth{1'b1}};

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

  sarq_t wsarq_in;
  logic  wsarq_push, wsarq_full;
  sarq_t rsarq_in;
  logic  rsarq_push, rsarq_full;
  axi_ar_chan_t write_index_ar,read_index_ar, read_data_ar;
  axi_r_chan_t write_index_r,read_index_r, read_data_r;
  logic write_index_ar_valid,read_index_ar_valid, read_data_ar_valid;
  logic write_index_ar_ready,read_index_ar_ready, read_data_ar_ready;
  logic write_index_r_valid,read_index_r_valid, read_data_r_valid;
  logic write_index_r_ready,read_index_r_ready, read_data_r_ready;

//******************************
//Mux of AR
//******************************
//0->read_data, 1->read_index, 2->write_index
  axi_ar_chan_t [2:0] mst_ar_chans;
  logic  [2:0] mst_ar_valids, mst_ar_readies;
  logic  [1:0] debug_choose_ar;

  assign mst_ar_chans[0] = read_data_ar;
  assign mst_ar_chans[1] = read_index_ar;
  assign mst_ar_chans[2] = write_index_ar;

  assign mst_ar_valids[0] = read_data_ar_valid;
  assign mst_ar_valids[1] = read_index_ar_valid;
  assign mst_ar_valids[2] = write_index_ar_valid;

  assign read_data_ar_ready = mst_ar_readies[0];
  assign read_index_ar_ready = mst_ar_readies[1];
  assign write_index_ar_ready = mst_ar_readies[2];

  rr_arb_tree #(
      .NumIn    ( 3 ),
      .DataType ( axi_ar_chan_t   ),
      .AxiVldRdy( 1'b1       ),
      .LockIn   ( 1'b1       )
    ) i_ar_mux (
      .clk_i  ( clk_i         ),
      .rst_ni ( ~rst_i        ),
      .flush_i( 1'b0          ),
      .rr_i   ( '0            ),
      .req_i  ( mst_ar_valids  ),
      .gnt_o  ( mst_ar_readies ),
      .data_i ( mst_ar_chans   ),
      .gnt_i  ( axi_rsp_i.ar_ready   ),
      .req_o  ( axi_req_o.ar_valid   ),
      .data_o ( axi_req_o.ar    ),
      .idx_o  ( debug_choose_ar )
    );

//******************************
//DEMux of R
//******************************
//0->read_data, 1->read_index, 2->write_index
  logic [1:0] debug_choose_r;
  assign debug_choose_r = (axi_rsp_i.r.id == WriteIndexId)? 2'b10:
                    (axi_rsp_i.r.id == ReadIndexId)? 2'b1:2'b0;
  always_comb begin
    read_data_r = '0;
    read_index_r = '0;
    write_index_r = '0;
    read_data_r_valid = '0;
    read_index_r_valid = '0;
    write_index_r_valid = '0;
    axi_req_o.r_ready = '0;
    if ((axi_rsp_i.r.id == WriteIndexId) & axi_rsp_i.r_valid) begin
      write_index_r = axi_rsp_i.r;
      write_index_r_valid = axi_rsp_i.r_valid;
      axi_req_o.r_ready = write_index_r_ready;
    end else if ((axi_rsp_i.r.id == ReadIndexId) & axi_rsp_i.r_valid) begin
      read_index_r = axi_rsp_i.r;
      read_index_r_valid = axi_rsp_i.r_valid;
      axi_req_o.r_ready = read_index_r_ready;
    end else begin
      read_data_r = axi_rsp_i.r;
      read_data_r_valid = axi_rsp_i.r_valid;
      axi_req_o.r_ready = read_data_r_ready;
    end
  end

//*********************
//ssr_to_std_aw
//*********************
axi_pack_conv_ax_all #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .NumInFlightTrans(NumInFlightWrite),
  .NumIndexBeat(NumIndexBeat),
  .NumIndexBeatMargin(NumIndexBeatMargin),
  .IndexFetchQueueDeepth(IndexFetchQueueDeepth),
  .AxiIdWidth(AxiIdWidth),
  .IndexId(WriteIndexId),
  .axi_ssr_ax_chan_t(axi_ssr_aw_chan_t),
  .axi_ax_chan_t(axi_aw_chan_t),
  .axi_index_ar_chan_t(axi_ar_chan_t),
  .axi_index_r_chan_t(axi_r_chan_t),
  .sarq_t(sarq_t)
  )i_ssr_to_std_aw(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //ssr
  .ax_ssr_chan_i(axi_ssr_req_i.aw),
  .ax_ssr_valid(axi_ssr_req_i.aw_valid),
  .ax_ssr_ready(axi_ssr_rsp_o.aw_ready),
  //sarq
  .xsarq_o     (wsarq_in),
  .xsarq_push   (wsarq_push),
  .xsarq_full   (wsarq_full),
  //std axi
  .ax_valid_o   (axi_req_o.aw_valid),
  .ax_ready_i   (axi_rsp_i.aw_ready),
  .ax_chan_o    (axi_req_o.aw),  
  //Index AR
  .index_ar_valid_o (write_index_ar_valid),
  .index_ar_ready_i (write_index_ar_ready),
  .index_ar_chan_o  (write_index_ar),
  //Index R
  .index_r_valid_i(write_index_r_valid),
  .index_r_ready_o(write_index_r_ready), 
  .index_r_chan_i(write_index_r)
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

//*********************
//ssr_to_std_ar
//*********************
axi_pack_conv_ax_all #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .NumInFlightTrans(NumInFlightRead),
  .NumIndexBeat(NumIndexBeat),
  .NumIndexBeatMargin(NumIndexBeatMargin),
  .IndexFetchQueueDeepth(IndexFetchQueueDeepth),
  .AxiIdWidth(AxiIdWidth),
  .IndexId(ReadIndexId),
  .axi_ssr_ax_chan_t(axi_ssr_ar_chan_t),
  .axi_ax_chan_t(axi_ar_chan_t),
  .axi_index_ar_chan_t(axi_ar_chan_t),
  .axi_index_r_chan_t(axi_r_chan_t),
  .sarq_t(sarq_t)
  )i_ssr_to_std_ar(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //ssr
  .ax_ssr_chan_i(axi_ssr_req_i.ar),
  .ax_ssr_valid(axi_ssr_req_i.ar_valid),
  .ax_ssr_ready(axi_ssr_rsp_o.ar_ready),
  //sarq
  .xsarq_o      (rsarq_in),
  .xsarq_push   (rsarq_push),
  .xsarq_full   (rsarq_full),
  //std axi
  .ax_valid_o   (read_data_ar_valid),
  .ax_ready_i   (read_data_ar_ready),
  .ax_chan_o    (read_data_ar),  
  //Index AR
  .index_ar_valid_o (read_index_ar_valid),
  .index_ar_ready_i (read_index_ar_ready),
  .index_ar_chan_o  (read_index_ar),
  //Index R
  .index_r_valid_i(read_index_r_valid),
  .index_r_ready_o(read_index_r_ready), 
  .index_r_chan_i(read_index_r)
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
  //std axi b channel
  .r_chan_i(read_data_r),
  .r_valid_i(read_data_r_valid),
  .r_ready_o(read_data_r_ready)
);


endmodule

