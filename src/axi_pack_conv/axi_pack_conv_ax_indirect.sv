//Author: Chi Zhang <chizhang@student.ethz.ch>

//1. Do not support sign extention of index

`include "common_cells/registers.svh"

module axi_pack_conv_ax_indirect #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I ,
  parameter int unsigned DataWidth_O ,
  parameter int unsigned AxiIdWidth,
  parameter logic [AxiIdWidth-1:0] IndexId,
  parameter int unsigned NumIndexBeat,
  parameter int unsigned NumIndexBeatMargin,
  parameter int unsigned IndexFetchQueueDeepth ,
  parameter type         axi_ax_chan_t    = logic,
  parameter type         axi_index_ar_chan_t    = logic,
  parameter type         axi_index_r_chan_t    = logic,
  parameter type         ssr_t    = logic,
  parameter type         sarq_t   = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //Buffer statue
  output logic                          empty_o,
  //ssr
  input ssr_t                           xrq_out,
  input logic                           xrq_empty,
  output logic                          xrq_pop,
  //sarq
  output sarq_t                         xsarq_in,
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
    logic [AxiIdWidth-1:0]          id;
    logic [AddrWidth-1:0]           addr;
    logic [DataAlign_O-1:0]         std_offset;
    axi_pkg::size_t                 index_size;
    logic [DataAlign_O-1:0]         last_index_offset;
    axi_pkg::size_t                 data_size;
    axi_pkg::len_t                  index_axi_len;
  } ifq_t; //index fetch queue


//********************
//Write Index Fetcher
//********************
  ifq_t xifq_from_fetcher,xifq_to_gen;
  logic xifq_start_from_fetcher, xifq_done_to_fetcher, xifq_start_to_gen, xifq_done_from_gen;
  logic index_fetcher_ar_valid,index_fetcher_ar_ready;
  axi_index_ar_chan_t index_fetcher_ar_chan;
  logic index_gen_r_valid,index_gen_r_ready;
  axi_index_r_chan_t index_gen_r_chan;

index_fetcher #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .AxiIdWidth(AxiIdWidth),
  .NumIndexBeat(NumIndexBeat),
  .axi_index_ar_chan_t(axi_index_ar_chan_t),
  .IndexId(IndexId),
  .ssr_t(ssr_t),
  .ifq_t(ifq_t)
  ) i_write_index_fetcher(
  .clk_i(clk_i),
  .rst_i (rst_i),
  //ssr
  .xrq_out(xrq_out),
  .xrq_empty(xrq_empty),
  .xrq_pop(xrq_pop),
  //ifq
  .xifq_in(xifq_from_fetcher),
  .xifq_start(xifq_start_from_fetcher),
  .xifq_done(xifq_done_to_fetcher),
  //std axi
  .index_ar_valid_o(index_fetcher_ar_valid),
  .index_ar_ready_i(index_fetcher_ar_ready),
  .index_ar_chan_o(index_fetcher_ar_chan)
);

//************************
//Index Fetch Queue
//************************

  ifq_t xifq_in;
  logic xifq_push, xifq_full;
  ifq_t xifq_out;
  logic xifq_pop, xifq_empty;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( IndexFetchQueueDeepth ),
    .dtype        ( ifq_t               )
  ) i_fifo_xifq (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (xifq_full),
    .empty_o (xifq_empty),
    .usage_o (/* open */),
    .data_i (xifq_in),
    .push_i (xifq_push),
    .data_o (xifq_out),
    .pop_i (xifq_pop)
  );

always_comb begin
  if (IndexFetchQueueDeepth > 0) begin
    xifq_in = xifq_from_fetcher;
    xifq_push = xifq_start_from_fetcher & ~xifq_full;
    xifq_pop = xifq_done_from_gen;
    xifq_done_to_fetcher = xifq_push & ~xifq_full;
    xifq_to_gen = xifq_out;
    xifq_start_to_gen = ~xifq_empty;
  end else begin
    xifq_done_to_fetcher = xifq_done_from_gen;
    xifq_to_gen = xifq_from_fetcher;
    xifq_start_to_gen = xifq_start_from_fetcher;
  end
end




//************************
//Write Data Standard Axi
//************************

indirect_std_axi_generater #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .axi_ax_chan_t(axi_ax_chan_t),
  .axi_index_r_chan_t(axi_index_r_chan_t),
  .ifq_t(ifq_t),
  .sarq_t(sarq_t)
  )i_write_std_axi_generater(
  .clk_i    (clk_i),
  .rst_i    (rst_i),
  //wifq
  .xifq_i   (xifq_to_gen),
  .xifq_start   (xifq_start_to_gen),
  .xifq_done   (xifq_done_from_gen),
  //sarq
  .xsarq_in     (xsarq_in),
  .xsarq_push   (xsarq_push),
  .xsarq_full   (xsarq_full),
  //index return
  .index_valid_i(index_gen_r_valid),
  .index_ready_o(index_gen_r_ready),
  .index_data (index_gen_r_chan),
  //data axi
  .ax_valid_o   (ax_valid_o),
  .ax_ready_i   (ax_ready_i),
  .ax_chan_o    (ax_chan_o)
);

credit_conunter #(
  .AddrWidth(AddrWidth),
  .DataWidth_I(DataWidth_I),
  .DataWidth_O(DataWidth_O),
  .MaxCredit(NumIndexBeat+NumIndexBeatMargin),
  .axi_index_ar_chan_t(axi_index_ar_chan_t),
  .axi_index_r_chan_t(axi_index_r_chan_t)
  )i_credit_conunter(
  .clk_i    (clk_i),
  .rst_i    (rst_i),
  //Index fetcher ar
  .index_fetcher_ar_valid_i(index_fetcher_ar_valid),
  .index_fetcher_ar_ready_o(index_fetcher_ar_ready),
  .index_fetcher_ar_i(index_fetcher_ar_chan),
  //Index ar
  .index_ar_valid_o(index_ar_valid_o),
  .index_ar_ready_i(index_ar_ready_i),
  .index_ar_chan_o(index_ar_chan_o),
  //index return
  .index_valid_i(index_r_valid_i),
  .index_ready_o(index_r_ready_o),
  .index_data (index_r_chan_i),
  //index data to generator
  .index_gen_valid_o(index_gen_r_valid),
  .index_gen_ready_i(index_gen_r_ready),
  .index_gen_data_o(index_gen_r_chan),
  //empty
  .empty_o(empty_o)
);

endmodule

module index_fetcher #(
  parameter int unsigned AddrWidth,
  parameter int unsigned DataWidth_I,
  parameter int unsigned DataWidth_O,
  parameter int unsigned AxiIdWidth,
  parameter logic [AxiIdWidth-1:0] IndexId,
  parameter int unsigned NumIndexBeat,
  parameter type         axi_index_ar_chan_t    = logic,
  parameter type         ssr_t    = logic,
  parameter type         ifq_t   = logic
)(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //ssr
  input ssr_t                           xrq_out,
  input logic                           xrq_empty,
  output logic                          xrq_pop,
  //ifq
  output ifq_t                          xifq_in,
  output logic                          xifq_start,
  input logic                           xifq_done,
  //to Credit Counter
  output logic                          index_ar_valid_o,
  input  logic                          index_ar_ready_i,
  output axi_index_ar_chan_t            index_ar_chan_o
);
  localparam int unsigned IndexOffsetWidth = $size(axi_pack_pkg::index_base_offset_t);
  localparam int unsigned ExtLen = $size(axi_pkg::len_t)+$size(axi_pkg::size_t);
  localparam int unsigned AddLen = $size(axi_pkg::len_t)+1;
  localparam int unsigned ExtWidth = AddrWidth-IndexOffsetWidth;
  localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
  localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);

  logic [AddrWidth-1:0] sign_ext_offset, real_addr,start_addr;
  logic [ExtLen-1:0] x_changed_len;
  logic [AddLen-1:0] total_beat,remian_beat,add_beat;
  logic [AddLen-1:0] index_cnt_q,index_cnt_d;
  `FFSR(index_cnt_q, index_cnt_d, '0, clk_i, rst_i)
  logic next_xrq, index_handshake;
  logic ifq_finish_q,ifq_finish_d;
  `FFSR(ifq_finish_q, ifq_finish_d, '0, clk_i, rst_i)

  //DataPath
  assign sign_ext_offset = {{ExtWidth{xrq_out.user.user.indirect.index_base_offset[IndexOffsetWidth-1]}},xrq_out.user.user.indirect.index_base_offset};
  assign start_addr = xrq_out.addr+sign_ext_offset;
  assign real_addr = index_cnt_q == 0 ? start_addr:
                     (((start_addr>>DataAlign_O)+{3'b0,index_cnt_q})<<DataAlign_O);

  assign index_ar_chan_o.addr = (real_addr>>DataAlign_O)<<DataAlign_O;
  assign index_ar_chan_o.size = DataAlign_O;
  assign index_ar_chan_o.len =  add_beat-1;
  assign index_ar_chan_o.id  = IndexId;
  assign    index_ar_chan_o.burst  = axi_pkg::BURST_INCR;
  assign    index_ar_chan_o.lock   = '0;//Never use atomic transaction
  assign    index_ar_chan_o.cache  = axi_pkg::CACHE_MODIFIABLE;
  assign    index_ar_chan_o.prot   = '0;
  assign    index_ar_chan_o.qos    = '0;
  assign    index_ar_chan_o.region = '0;
  assign    index_ar_chan_o.user   = '0;

  assign x_changed_len = start_addr[DataAlign_O-1:0]+({3'b0,xrq_out.len}<<xrq_out.user.user.indirect.index_size);
  assign total_beat = (x_changed_len>>DataAlign_O)+1;
  assign remian_beat = total_beat-index_cnt_q;
  assign add_beat = remian_beat > NumIndexBeat? NumIndexBeat : remian_beat;

  assign xifq_in = '{
    id:   xrq_out.id,
    addr: xrq_out.addr,
    std_offset: start_addr[DataAlign_O-1:0],
    index_size: xrq_out.user.user.indirect.index_size, 
    data_size: xrq_out.size, 
    last_index_offset: x_changed_len[DataAlign_O-1:0],
    index_axi_len: (x_changed_len>>DataAlign_O)
  };

  //Control Signal
  assign index_handshake = index_ar_valid_o & index_ar_ready_i;
  assign index_cnt_d = (xrq_pop | xrq_empty)? '0:
                        index_handshake?      index_cnt_q+add_beat:
                                              index_cnt_q;
  assign ifq_finish_d = (xrq_pop | xrq_empty)? '0:
                        (xifq_start & xifq_done)? 1:
                        ifq_finish_q;

  assign next_xrq = (index_cnt_q == total_beat);

  assign index_ar_valid_o = ~xrq_empty &  ~next_xrq;
  assign xrq_pop = next_xrq & (xifq_done | ifq_finish_q) & ~xrq_empty;
  assign xifq_start = ~xrq_empty & ~ifq_finish_q;

endmodule 

module indirect_std_axi_generater #(
  parameter int unsigned AddrWidth,
  parameter int unsigned DataWidth_I,
  parameter int unsigned DataWidth_O,
  parameter type         axi_ax_chan_t    = logic,
  parameter type         axi_index_r_chan_t    = logic,
  parameter type         ifq_t   = logic,
  parameter type         sarq_t    = logic
)(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //ifq
  input ifq_t                           xifq_i,
  input logic                           xifq_start,
  output logic                          xifq_done,
  //sarq
  output sarq_t                         xsarq_in,
  output logic                          xsarq_push,
  input logic                           xsarq_full,
  //index return
  input logic                           index_valid_i,
  output logic                          index_ready_o,
  input axi_index_r_chan_t              index_data,
  //Data axi
  output logic                          ax_valid_o,
  input  logic                          ax_ready_i,
  output axi_ax_chan_t                  ax_chan_o
);

  localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
  localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);
  localparam int unsigned StrbWidth = DataWidth_O/8;


/**********
Index to AX 
***********/
  axi_pkg::len_t  index_beats_cnt_q,index_beats_cnt_d;
  `FFSR(index_beats_cnt_q, index_beats_cnt_d, '0, clk_i, rst_i)
  logic [DataAlign_O-1:0] cnt_q,cnt_d;
  `FFSR(cnt_q, cnt_d, '0, clk_i, rst_i)
  logic [DataAlign_O-1:0] index_offset, ref_ind;
  logic index_handshake, data_handshake;
  logic complete_current_index_beat;
  logic [StrbWidth-1:0] strib,ref_strib;
  logic [DataWidth_O-1:0] index,index_data_mask;
  //Contorl Signal
  assign index_handshake = index_valid_i & index_ready_o;
  assign data_handshake  = ax_valid_o & ax_ready_i;
  assign ref_ind = 1;
  assign complete_current_index_beat = ((index_beats_cnt_q == xifq_i.index_axi_len) & (index_offset==xifq_i.last_index_offset)) | (index_offset+(ref_ind<<xifq_i.index_size) == '0);
  assign index_ready_o = index_valid_i & xifq_start & complete_current_index_beat & data_handshake;
  assign xifq_done = index_handshake & ((index_beats_cnt_q == xifq_i.index_axi_len) & (index_offset==xifq_i.last_index_offset));
  assign ax_valid_o = xifq_start & ~xsarq_full & index_valid_i;
  assign xsarq_push = data_handshake & ~xsarq_full;
  assign cnt_d =  (xifq_done | ~xifq_start)?  '0:
                  data_handshake?       cnt_q+1:
                                        cnt_q;
  assign index_beats_cnt_d = (xifq_done | ~xifq_start)?   '0:
                              index_handshake?            index_beats_cnt_q+1:
                                                          index_beats_cnt_q;

  //Datapath
  assign index_offset = xifq_i.std_offset+(cnt_q<<xifq_i.index_size);
  assign ref_strib = 1;
  assign strib = {StrbWidth{1'b1}}>>(StrbWidth-(ref_strib<<xifq_i.index_size));
  genvar gi;
    for (gi = 0; gi < DataWidth_O; gi++) begin
      assign index_data_mask[gi] = strib[gi/8];
    end
  assign index = (index_data.data>>{index_offset,3'b0})&index_data_mask;

  always_comb begin
    ax_chan_o = '0;
    ax_chan_o.addr = xifq_i.addr+({3'b0,index}<<xifq_i.data_size);
    ax_chan_o.len = 0;
    ax_chan_o.size = xifq_i.data_size;
    ax_chan_o.id = xifq_i.id;
    ax_chan_o.burst  = axi_pkg::BURST_INCR;
    ax_chan_o.lock   = '0;//Never use atomic transaction
    ax_chan_o.cache  = axi_pkg::CACHE_MODIFIABLE;
    ax_chan_o.prot   = '0;
    ax_chan_o.qos    = '0;
    ax_chan_o.region = '0;
    ax_chan_o.user   = '0;
  end

	assign xsarq_in = '{
    id: xifq_i.id,
		ssr_offset: xifq_i.addr[DataAlign_I-1:0]+(cnt_q<<ax_chan_o.size),
		ssr_size: xifq_i.data_size,
		ssr_stride: 0,
		std_offset: ax_chan_o.addr[DataAlign_O-1:0],
		same_size: 1,
		ssr_len: 0,
		ssr_last:  xifq_done
	};


endmodule 

module credit_conunter #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I ,
  parameter int unsigned DataWidth_O ,
  parameter int unsigned MaxCredit ,
  parameter type         axi_index_ar_chan_t   = logic,
  parameter type         axi_index_r_chan_t    = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //Index fetcher ar
  input logic                           index_fetcher_ar_valid_i,
  output logic                          index_fetcher_ar_ready_o,
  input axi_index_ar_chan_t             index_fetcher_ar_i,
  //Index ar
  output logic                          index_ar_valid_o,
  input  logic                          index_ar_ready_i,
  output axi_index_ar_chan_t            index_ar_chan_o,
  //index return
  input logic                           index_valid_i,
  output logic                          index_ready_o,
  input axi_index_r_chan_t              index_data,
  //index data to generator
  output logic                          index_gen_valid_o,
  input logic                           index_gen_ready_i,
  output axi_index_r_chan_t             index_gen_data_o,
  //empty
  output logic                          empty_o
);

localparam int unsigned ADDR_DEPTH   = (MaxCredit > 1) ? $clog2(MaxCredit) : 1;

/**********
Index Buffer
***********/
  typedef logic [DataWidth_O-1:0] ind_t;
  ind_t ind_in, ind_out;
  logic ind_push, ind_pop, ind_full, ind_empty;
  logic  [ADDR_DEPTH-1:0] usage;

  fifo_v3 #(
    .FALL_THROUGH ( 1'b0                ),
    .DEPTH        ( MaxCredit ),
    .dtype        ( ind_t               )
  ) i_fifo_ind (
    .clk_i,
    .rst_ni (~rst_i),
    .flush_i (1'b0),
    .testmode_i(1'b0),
    .full_o (ind_full),
    .empty_o (ind_empty),
    .usage_o (usage),
    .data_i (ind_in),
    .push_i (ind_push),
    .data_o (ind_out),
    .pop_i (ind_pop)
  );

//Control Signal
  logic index_fetcher_handshack, gen_handshcak;
  logic allow_index_fetcher;
  logic [ADDR_DEPTH:0] credit_cnt_q,credit_cnt_d;
  `FFSR(credit_cnt_q, credit_cnt_d, '0, clk_i, rst_i)
  assign index_fetcher_handshack = index_fetcher_ar_valid_i & index_fetcher_ar_ready_o;
  assign gen_handshcak = index_gen_valid_o & index_gen_ready_i;
  assign credit_cnt_d = (index_fetcher_handshack & gen_handshcak)? credit_cnt_q+index_ar_chan_o.len:
                        (index_fetcher_handshack & ~gen_handshcak)? credit_cnt_q+index_ar_chan_o.len+1:
                        (~index_fetcher_handshack & gen_handshcak)? credit_cnt_q-1:
                                                                    credit_cnt_q;
  assign allow_index_fetcher = ((MaxCredit-credit_cnt_q) >= (index_ar_chan_o.len+1));
  assign index_ar_valid_o =  index_fetcher_ar_valid_i & allow_index_fetcher;
  assign index_fetcher_ar_ready_o = index_ar_valid_o & index_ar_ready_i;

  assign index_ready_o = ~ind_full;
  assign ind_push = index_valid_i & index_ready_o;

  assign index_gen_valid_o = ~ind_empty;
  assign ind_pop = gen_handshcak;

  assign empty_o = (credit_cnt_q == 0);

//Datapath
  assign index_ar_chan_o = index_fetcher_ar_i;
  assign ind_in = index_data.data;
  always_comb begin
    index_gen_data_o = '0;
    index_gen_data_o.data = ind_out;
  end

endmodule 