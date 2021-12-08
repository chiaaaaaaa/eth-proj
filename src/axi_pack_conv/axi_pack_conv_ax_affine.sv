//Author: Chi Zhang <chizhang@student.ethz.ch>

//1. Only support INC BURST
//2. Do not support atomic transaction
//3. Always trust last signal for Write and Read.
//4. Input and Output AddrWidth equal to each other
//5. Do not support Read Transaction with different IDs
//6. The Output DataWidth must >= Input DataWidth 
//7. Do not support misalignment transfer

/*
Todo: 
1. support signed stride
*/

`include "common_cells/registers.svh"

module axi_pack_conv_ax_affine #(
  parameter int unsigned AddrWidth ,
  parameter int unsigned DataWidth_I ,
  parameter int unsigned DataWidth_O ,
  parameter type         axi_ax_chan_t    = logic,
  parameter type         ssr_t    = logic,
  parameter type         sarq_t   = logic
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
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
  output axi_ax_chan_t                  ax_chan_o 
);
  localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
  localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);

  axi_pack_pkg::len_t x_transfer_beats_per_axi,x_transfer_beats_per_axi_sub;
  axi_pack_pkg::len_t x_cnt_d,x_cnt_q;
  `FFSR(x_cnt_q, x_cnt_d, '0, clk_i, rst_i)
  typedef enum logic [2:0] {DIRECT_THROUGH,LOOP_ONE_BY_ONE,LOOP_BURST,NEST_ONE_BY_ONE,NEST_BURST} strategy_t;
  strategy_t x_strategy; 
  logic ax_output_shake;
  logic gx_load_addr, gx_next_addr;
  axi_pack_pkg::nest_len_t gx_nest_len;
  logic  [AddrWidth-1:0]        x_real_addr,gx_addr;
  axi_pkg::len_t                x_changed_len;
  axi_pack_pkg::affine_t     x_affine;

  //Control signal
  assign x_affine = xrq_out.user.user.affine;
  always_comb begin
    logic [DataAlign_O:0] reference;
    reference = {1'b1,{DataAlign_O{1'b0}}};
    x_strategy = DIRECT_THROUGH;
    if (~((x_affine.nest_len==0) & (x_affine.stride==0))) begin
      if (x_affine.nest_len==0) begin
        x_strategy = (({3'b0,x_affine.stride}<<xrq_out.size)<reference)? LOOP_BURST:LOOP_ONE_BY_ONE;
      end else begin
        x_strategy = (({3'b0,x_affine.nest_stride}<<xrq_out.size)<reference)? NEST_BURST:NEST_ONE_BY_ONE;
      end
    end 
  end

  always_comb begin
    case (x_strategy)
      DIRECT_THROUGH: x_transfer_beats_per_axi_sub = xrq_out.len;
      LOOP_ONE_BY_ONE: x_transfer_beats_per_axi_sub = 0;
      LOOP_BURST: x_transfer_beats_per_axi_sub = xrq_out.len;
      NEST_ONE_BY_ONE: x_transfer_beats_per_axi_sub = 0;
      NEST_BURST: begin 
        if ((xrq_out.len-x_cnt_q)<x_affine.nest_len) x_transfer_beats_per_axi_sub = (xrq_out.len-x_cnt_q);
        else x_transfer_beats_per_axi_sub = x_affine.nest_len;
      end
      default : x_transfer_beats_per_axi_sub = xrq_out.len;
    endcase
  end

  assign x_transfer_beats_per_axi = x_transfer_beats_per_axi_sub +1;

  always_comb begin
    case (x_strategy)
      DIRECT_THROUGH: gx_nest_len = 0;
      LOOP_ONE_BY_ONE: gx_nest_len = 0;
      LOOP_BURST: gx_nest_len = 0;
      NEST_ONE_BY_ONE: gx_nest_len = x_affine.nest_len;
      NEST_BURST: gx_nest_len = 0;
      default : gx_nest_len = 0;
    endcase
  end


  always_comb begin
    logic [DataAlign_O:0] mult_stride,mult_nest_stride;
    mult_stride = x_affine.stride+1;
    mult_nest_stride = x_affine.nest_stride+1;
    case (x_strategy)
      DIRECT_THROUGH: x_changed_len = xrq_out.len;
      LOOP_ONE_BY_ONE: x_changed_len = 0;
      LOOP_BURST: x_changed_len = (x_real_addr[DataAlign_O-1:0]+({3'b0,({3'b0,xrq_out.len}*mult_stride)}<<xrq_out.size))>>DataAlign_O;
      NEST_ONE_BY_ONE: x_changed_len = 0;
      NEST_BURST: x_changed_len = (x_real_addr[DataAlign_O-1:0]+({3'b0,({3'b0,x_transfer_beats_per_axi_sub}*mult_nest_stride)}<<xrq_out.size))>>DataAlign_O;
      default : x_changed_len = xrq_out.len;
    endcase
  end

  assign gx_load_addr = ax_output_shake & (x_cnt_q == 0);
  assign gx_next_addr = ax_output_shake;

  addr_generator #(
  .AddrWidth(AddrWidth)
  ) i_addr_generator (
    .clk_i(clk_i),
    .rst_i (rst_i),
  //Input Data
  .addr_i(xrq_out.addr),
  .nest_len(gx_nest_len),
  .nest_stride(x_affine.nest_stride),
  .stride(x_affine.stride),
  .size(xrq_out.size),
  //Control signal
  .load(gx_load_addr),
  .next(gx_next_addr),
  //Output Data
  .addr_o(gx_addr)
);

  assign x_real_addr = (x_cnt_q == 0)? xrq_out.addr:gx_addr;

  assign ax_output_shake = ax_valid_o & ax_ready_i;

  assign x_cnt_d = (xrq_pop | xrq_empty)?  '0:
                    ax_output_shake?        x_cnt_q+x_transfer_beats_per_axi:
                                            x_cnt_q; 

  axi_pack_pkg::len_t total_cnt;
  assign total_cnt = xrq_out.len+1;

  assign xrq_pop = ax_output_shake &
                   ~xrq_empty & 
                   (x_cnt_q == total_cnt-x_transfer_beats_per_axi) ;

  assign ax_valid_o = ~xrq_empty & ~xsarq_full;
  assign xsarq_push = ax_output_shake & ~xsarq_full;


  //Datapath
  always_comb begin
    ax_chan_o = '0; //defualt
    ax_chan_o.addr = (x_strategy == LOOP_BURST || x_strategy == NEST_BURST)? (x_real_addr>>DataAlign_O)<<DataAlign_O:x_real_addr;
    ax_chan_o.len  = x_changed_len;
    ax_chan_o.size = (x_strategy == LOOP_BURST || x_strategy == NEST_BURST)? DataAlign_O:xrq_out.size;
    ax_chan_o.burst  = axi_pkg::BURST_INCR;
    ax_chan_o.lock   = '0;//Never use atomic transaction
    ax_chan_o.cache  = axi_pkg::CACHE_MODIFIABLE;
    ax_chan_o.id     = xrq_out.id;
    ax_chan_o.prot   = '0;
    ax_chan_o.qos    = '0;
    ax_chan_o.region = '0;
    ax_chan_o.user   = '0;
  end

  assign xsarq_in.id = xrq_out.id;
  assign xsarq_in.ssr_size = xrq_out.size;
  assign xsarq_in.ssr_stride = (x_strategy == LOOP_BURST)? x_affine.stride:
                               (x_strategy == NEST_BURST)? x_affine.nest_stride:
                                                           0;
  assign xsarq_in.ssr_offset = (xrq_out.addr+(x_cnt_q<<xrq_out.size));
  assign xsarq_in.same_size = (ax_chan_o.size == xrq_out.size);
  assign xsarq_in.std_offset = x_real_addr[DataAlign_O-1:0];
  assign xsarq_in.ssr_last = (x_cnt_q == total_cnt-x_transfer_beats_per_axi);
  always_comb begin
    case (x_strategy)
      DIRECT_THROUGH: xsarq_in.ssr_len = xrq_out.len;
      LOOP_ONE_BY_ONE: xsarq_in.ssr_len = 0;
      LOOP_BURST: xsarq_in.ssr_len = xrq_out.len;
      NEST_ONE_BY_ONE: xsarq_in.ssr_len = 0;
      NEST_BURST: xsarq_in.ssr_len = x_transfer_beats_per_axi_sub;
      default : xsarq_in.ssr_len = xrq_out.len;
    endcase
  end

endmodule 


module addr_generator #(
  parameter int unsigned AddrWidth 
  )(
  input  logic                          clk_i,
  input  logic                          rst_i,
  //Input Data
  input  logic [AddrWidth-1:0]          addr_i,
  input axi_pack_pkg::nest_len_t     nest_len,
  input axi_pack_pkg::nest_stride_t       nest_stride,
  input axi_pack_pkg::stride_t       stride,
  input axi_pkg::size_t         size,
  //Control signal
  input logic                           load,
  input logic                           next,
  //Output Data
  output logic  [AddrWidth-1:0]         addr_o
);
//**********
//Definition
//**********
  //Counter
  logic next_loop;
  axi_pack_pkg::nest_len_t  nest_cnt_d,nest_cnt_q;
  `FFSR(nest_cnt_q, nest_cnt_d, '0, clk_i, rst_i)
  //Register
  logic [AddrWidth-1:0]    loop_start_addr_d,loop_start_addr_q,pointer_d,pointer_q;
  `FFSR(loop_start_addr_q, loop_start_addr_d, '0, clk_i, rst_i)
  `FFSR(pointer_q, pointer_d, '0, clk_i, rst_i)
  //Adder
  logic [AddrWidth-1:0]         adder_in_left,adder_in_right,adder_out;
//**********
//Assign
//**********
  //Counter
  assign nest_cnt_d = (load & next & (nest_len!=0)) ?      1:
                      load?                               '0:
                      (next_loop&next)?                   '0: 
                      next ?                    nest_cnt_q+1:
                                                  nest_cnt_q;
  assign next_loop = (nest_cnt_q==nest_len);

  //Register
  assign loop_start_addr_d =  (load & next & (nest_len!=0)) ?      addr_i:
                              (load & next & (nest_len==0)) ?      adder_out://addr_i+((stride+1)<<size):
                              load?                                addr_i:
                              (next_loop&next)?                    adder_out://loop_start_addr_q+((stride+1)<<size):
                              next?                                loop_start_addr_q:
                                                                   loop_start_addr_q;

  assign pointer_d         =  (load & next & (nest_len!=0)) ?      adder_out://addr_i+((nest_stride+1)<<size):
                              (load & next & (nest_len==0)) ?      adder_out://addr_i+((stride+1)<<size):
                              load?                                addr_i:
                              (next_loop&next)?                    adder_out://loop_start_addr_q+((stride+1)<<size):
                              next?                                adder_out://pointer_q+((nest_stride+1)<<size):
                                                                   pointer_q;
  //Adder
  assign adder_out = adder_in_left + adder_in_right;

  assign adder_in_left      = (load & next & (nest_len!=0)) ?      addr_i:
                              (load & next & (nest_len==0)) ?      addr_i:
                              load?                                '0:
                              (next_loop&next)?                    loop_start_addr_q:
                              next?                                pointer_q:
                                                                   '0;

  assign adder_in_right     = (load & next & (nest_len!=0)) ?      ((nest_stride+1)<<size):
                              (load & next & (nest_len==0)) ?      ((stride+1)<<size):
                              load?                                '0:
                              (next_loop&next)?                    ((stride+1)<<size):
                              next?                                ((nest_stride+1)<<size):
                                                                   '0;

  //Output
  assign addr_o = pointer_q;

endmodule 