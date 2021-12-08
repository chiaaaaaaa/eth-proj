//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "common_cells/registers.svh"

module axi_pack_conv_ax_all #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I,
  parameter int unsigned DataWidth_O,
  parameter int unsigned NumInFlightTrans,
  parameter int unsigned NumIndexBeat,
  parameter int unsigned NumIndexBeatMargin,
  parameter int unsigned IndexFetchQueueDeepth ,
  parameter int unsigned AxiIdWidth,
  parameter logic [AxiIdWidth-1:0] IndexId,
  parameter type         axi_ssr_ax_chan_t    = logic,
  parameter type         axi_ax_chan_t    = logic,
  parameter type         axi_index_ar_chan_t    = logic,
  parameter type         axi_index_r_chan_t    = logic,
  parameter type         sarq_t   = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //ssr axi
  input axi_ssr_ax_chan_t               ax_ssr_chan_i,
  input logic                           ax_ssr_valid,
  output logic                          ax_ssr_ready,
  //sarq
  output sarq_t                         xsarq_o,
  output logic                          xsarq_push,
  input logic                           xsarq_full,
  //std axi
  output logic                          ax_valid_o,
  input  logic                          ax_ready_i,
  output axi_ax_chan_t                  ax_chan_o,
  //Index AR
  output logic                          index_ar_valid_o,
  input  logic                          index_ar_ready_i,
  output axi_index_ar_chan_t            index_ar_chan_o,
  //Index R
  input logic                           index_r_valid_i,
  output logic                          index_r_ready_o, 
  input axi_index_r_chan_t              index_r_chan_i
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


//************************
//Request Queue (WRQ)
//************************

ssr_t xrq_in, xrq_out;
logic xrq_valid, xrq_ready;

assign xrq_in = '{
    id:         ax_ssr_chan_i.id,
    addr:       ax_ssr_chan_i.addr,
    size:       ax_ssr_chan_i.size,
    len:        ax_ssr_chan_i.len,
    user:       ax_ssr_chan_i.user 
  };      

if (NumInFlightTrans>1) begin
  logic xrq_empty,xrq_pop, xrq_full, xrq_push;
  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( NumInFlightTrans ),
    .dtype        ( ssr_t               )
  ) i_fifo_xrq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (xrq_full),
    .empty_o (xrq_empty),
    .usage_o (/* open */),
    .data_i (xrq_in),
    .push_i (xrq_push),
    .data_o (xrq_out),
    .pop_i (xrq_pop)
  );
  assign xrq_push = ~xrq_full & ax_ssr_valid;
  assign ax_ssr_ready = xrq_push;
  assign xrq_valid = ~xrq_empty;
  assign xrq_pop = xrq_ready;
end else begin
  spill_register #(
      .T       ( ssr_t      ),
      .Bypass  ( NumInFlightTrans<1              ) // because module param indicates if we want a spill reg
    ) i_aw_spill_reg (
      .clk_i   ( clk_i                 ),
      .rst_ni  ( ~rst_i                ),
      .valid_i ( ax_ssr_valid    ),
      .ready_o ( ax_ssr_ready   ),
      .data_i  ( xrq_in ),
      .valid_o ( xrq_valid          ),
      .ready_i ( xrq_ready          ),
      .data_o  ( xrq_out    )
    );
end

  

//************************
//SSR AX DEMUX
//************************
  ssr_t affine_ssr, indirect_ssr;
  logic ax_indirect_fifo_empty;
  logic affine_valid,affine_ready,indirect_valid,indirect_ready;

  ssr_ax_mux #(
    .ssr_t(ssr_t)
  )i_ssr_ax_mux(
  .clk_i              (clk_i),
  .rst_i              (rst_i),
  //SSR AX
  .ssr_ax_i           (xrq_out),
  .ssr_valid_i        (xrq_valid),
  .ssr_ready_o        (xrq_ready),
  //To affine
  .affine_ax_o        (affine_ssr),
  .affine_valid_o     (affine_valid),
  .affine_ready_i     (affine_ready),
  //To indirect
  .indirect_ax_o      (indirect_ssr),
  .indirect_valid_o   (indirect_valid),
  .indirect_ready_i   (indirect_ready),
  .indirect_fifo_empty(ax_indirect_fifo_empty)
  );

//******************************
//Affine to Standard Axi request generator
//******************************
  axi_ax_chan_t ax_affine;
  logic ax_affine_valid,ax_affine_ready;
  sarq_t xsarq_in_affine;
  logic  xsarq_push_affine, xsarq_full_affine;
  axi_pack_conv_ax_affine #(
    .AddrWidth(AddrWidth),
    .DataWidth_I(DataWidth_I),
    .DataWidth_O(DataWidth_O),
    .axi_ax_chan_t(axi_ax_chan_t),
    .ssr_t(ssr_t),
    .sarq_t(sarq_t)
    ) i_affine_to_std_aw(
    .clk_i(clk_i),
    .rst_i (rst_i),
    //ssr
    .xrq_out(affine_ssr),
    .xrq_empty(~affine_valid),
    .xrq_pop(affine_ready),
    //sarq
    .xsarq_in(xsarq_in_affine),
    .xsarq_push(xsarq_push_affine),
    .xsarq_full(xsarq_full_affine),
    //std axi
    .ax_valid_o(ax_affine_valid),
    .ax_ready_i(ax_affine_ready),
    .ax_chan_o(ax_affine)  
  );

//******************************
//Indirect to Standard Axi request generator
//******************************
  axi_ax_chan_t ax_indirect;
  logic ax_indirect_valid,ax_indirect_ready;
  axi_index_ar_chan_t ar_index;
  logic ar_index_valid,ar_index_ready;
  axi_index_r_chan_t r_index;
  logic r_index_valid,r_index_ready;
  sarq_t xsarq_in_indirect;
  logic  xsarq_push_indirect, xsarq_full_indirect;

axi_pack_conv_ax_indirect #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .NumIndexBeat(NumIndexBeat),
  .NumIndexBeatMargin(NumIndexBeatMargin),
  .IndexFetchQueueDeepth(IndexFetchQueueDeepth) ,
  .AxiIdWidth(AxiIdWidth),
  .IndexId(IndexId),
  .axi_ax_chan_t(axi_ax_chan_t),
  .axi_index_ar_chan_t(axi_index_ar_chan_t),
  .axi_index_r_chan_t(axi_index_r_chan_t),
  .ssr_t(ssr_t),
  .sarq_t(sarq_t)
  )i_indirect_to_std_ax(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //Buffer statue
  .empty_o(ax_indirect_fifo_empty),
  //ssr
  .xrq_out(indirect_ssr),
  .xrq_empty(~indirect_valid),
  .xrq_pop(indirect_ready),
  //sarq
  .xsarq_in     (xsarq_in_indirect),
  .xsarq_push   (xsarq_push_indirect),
  .xsarq_full   (xsarq_full_indirect),
  //std axi
  .ax_valid_o(ax_indirect_valid),
  .ax_ready_i(ax_indirect_ready),
  .ax_chan_o(ax_indirect),  
  //Index AR
  .index_ar_valid_o(ar_index_valid),
  .index_ar_ready_i(ar_index_ready),
  .index_ar_chan_o(ar_index),
  //Index R
  .index_r_valid_i(r_index_valid),
  .index_r_ready_o(r_index_ready), 
  .index_r_chan_i(r_index)
);

//******************************
//Index AR
//******************************

assign index_ar_chan_o = ar_index;
assign index_ar_valid_o = ar_index_valid;
assign ar_index_ready = index_ar_ready_i;

//******************************
//Index R
//******************************

assign r_index = index_r_chan_i;
assign r_index_valid = index_r_valid_i;
assign index_r_ready_o = r_index_ready;

//******************************
//Mux of AW
//******************************
  axi_ax_chan_t [1:0] mst_ax_chans;
  logic  [1:0] mst_ax_valids, mst_ax_readies;
  logic debug_choose_ax;

  assign mst_ax_chans[0] = ax_affine;
  assign mst_ax_chans[1] = ax_indirect;
  assign mst_ax_valids[0] = ax_affine_valid;
  assign mst_ax_valids[1] = ax_indirect_valid;
  assign ax_affine_ready = mst_ax_readies[0];
  assign ax_indirect_ready = mst_ax_readies[1];

  rr_arb_tree #(
      .NumIn    ( 2 ),
      .DataType ( axi_ax_chan_t   ),
      .AxiVldRdy( 1'b1       ),
      .LockIn   ( 1'b1       )
    ) i_aw_mux (
      .clk_i  ( clk_i         ),
      .rst_ni ( ~rst_i        ),
      .flush_i( 1'b0          ),
      .rr_i   ( '0            ),
      .req_i  ( mst_ax_valids  ),
      .gnt_o  ( mst_ax_readies ),
      .data_i ( mst_ax_chans   ),
      .gnt_i  ( ax_ready_i   ),
      .req_o  ( ax_valid_o   ),
      .data_o ( ax_chan_o    ),
      .idx_o  ( debug_choose_ax )
    );

//******************************
//Demux of WSARQ
//******************************
  sarq_t [1:0] mst_xsarq_chans;
  logic  [1:0] mst_xsarq_valids, mst_xsarq_readies;

  assign mst_xsarq_chans[0] = xsarq_in_affine;
  assign mst_xsarq_chans[1] = xsarq_in_indirect;
  assign mst_xsarq_valids[0] = xsarq_push_affine;
  assign mst_xsarq_valids[1] = xsarq_push_indirect;
  assign xsarq_full_affine = xsarq_full;
  assign xsarq_full_indirect = xsarq_full;

  rr_arb_tree #(
      .NumIn    ( 2 ),
      .DataType ( sarq_t   ),
      .AxiVldRdy( 1'b0       ),
      .LockIn   ( 1'b1       )
    ) i_wsarq_mux (
      .clk_i  ( clk_i         ),
      .rst_ni ( ~rst_i        ),
      .flush_i( 1'b0          ),
      .rr_i   ( '0            ),
      .req_i  ( mst_xsarq_valids  ),
      .gnt_o  ( mst_xsarq_readies ),
      .data_i ( mst_xsarq_chans   ),
      .gnt_i  ( ~xsarq_full   ),
      .req_o  ( xsarq_push   ),
      .data_o ( xsarq_o    ),
      .idx_o  (  )
    );
 
endmodule 


module ssr_ax_mux #(
  parameter type         ssr_t    = logic
)(
	input  logic                          clk_i,
  	input  logic                          rst_i,
  	//Input AX
  	input  ssr_t 						  ssr_ax_i,
  	input  logic 						  ssr_valid_i,
  	output logic 						  ssr_ready_o,
  	//To affine
  	output  ssr_t 						  affine_ax_o,
  	output logic 						  affine_valid_o,
  	input logic 						  affine_ready_i,
  	//To indirect
  	output  ssr_t 						  indirect_ax_o,
  	output logic 						  indirect_valid_o,
  	input logic 						  indirect_ready_i,
  	//check empty
  	input logic 						  indirect_fifo_empty
);
	
	logic  indirect_enable_q,indirect_enable_d;
	`FFSR(indirect_enable_q, indirect_enable_d, '0, clk_i, rst_i)
	assign indirect_enable_d = (ssr_valid_i & ssr_ready_o)? ssr_ax_i.user.indirect_enable:indirect_enable_q;
	logic flush_indirect_fifo;
	assign flush_indirect_fifo = ssr_ax_i & indirect_enable_q & ~ssr_ax_i.user.indirect_enable & ~indirect_fifo_empty;
	//Control signal
	assign affine_valid_o = flush_indirect_fifo? 			0: 
							ssr_ax_i.user.indirect_enable? 	0:
															ssr_valid_i;
	assign indirect_valid_o = ssr_ax_i.user.indirect_enable? ssr_valid_i:0;
	assign ssr_ready_o = 	flush_indirect_fifo? 			0:
							ssr_ax_i.user.indirect_enable?	indirect_ready_i:
															affine_ready_i;
	//Datapath
	assign affine_ax_o = ssr_ax_i;
	assign indirect_ax_o = ssr_ax_i;


endmodule 