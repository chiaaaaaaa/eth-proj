//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "reqrsp_interface/typedef.svh"
`include "reqrsp_interface/assign.svh"
`include "axi/assign.svh"
`include "axi/typedef.svh"
`include "axi_ssr/typedef.svh"
`include "axi_ssr/assign.svh"

`timescale 1ns/1ns

module tb_ssr_lsu_and_filter_evaluation;
// ------------
//  Parameters
// ------------

//clock parameter
localparam time ClkPeriod = 10ns;
localparam time ApplTime =  0ns;
localparam time TestTime =  10ns;

//Set up snitch lsu parameters
localparam int unsigned AddrWidth = 32;
localparam int unsigned DataWidth_all = 32;
//input axi
localparam int unsigned DataWidth_I = DataWidth_all;
localparam int unsigned DataAlign_I = $clog2(DataWidth_I/8);
localparam int unsigned AxiUserWidth_I = $bits(axi_pack_pkg::ssr_user_t);
localparam int unsigned AxiIdWidth_I = 2;
localparam int unsigned AxiStrbWidth_I = DataWidth_I / 8;
//output axi
localparam int unsigned DataWidth_O = DataWidth_all;
localparam int unsigned DataAlign_O = $clog2(DataWidth_O/8);
localparam int unsigned AxiUserWidth_O = 1;
localparam int unsigned AxiIdWidth_O = 2;
localparam int unsigned AxiStrbWidth_O = DataWidth_O / 8;

localparam int unsigned NumIntOutstandingStores = 32;
localparam int unsigned NumIntOutstandingLoads = 32;
localparam bit          AxiMemWarnUninitialized = 1'b0;
localparam int unsigned InputDelay = 0;
localparam int unsigned OutputDelay = 5;


//define types 
typedef  logic[4:0] tag_t;
typedef  logic[AddrWidth-1:0] addr_t; 
typedef  logic[DataWidth_I-1:0] lsu_data_t; 
`LSU_TYPEDEF_ALL(lsu, addr_t, lsu_data_t)

typedef logic [AddrWidth-1:0] axi_addr_t;

typedef logic [DataWidth_I-1:0] axi_in_data_t;
typedef logic [AxiIdWidth_I-1:0]   axi_in_id_t;
typedef logic [AxiStrbWidth_I-1:0] axi_in_strb_t;
typedef logic [AxiUserWidth_I-1:0] axi_in_user_t;
typedef logic [DataWidth_O-1:0] axi_out_data_t;
typedef logic [AxiIdWidth_O-1:0]   axi_out_id_t;
typedef logic [AxiStrbWidth_O-1:0] axi_out_strb_t;
typedef logic [AxiUserWidth_O-1:0] axi_out_user_t;

`AXI_TYPEDEF_AW_CHAN_T(axi_in_aw_t, axi_addr_t, axi_in_id_t, axi_in_user_t)
`AXI_TYPEDEF_W_CHAN_T(axi_in_w_t, axi_in_data_t, axi_in_strb_t, axi_out_user_t)
`AXI_TYPEDEF_B_CHAN_T(axi_in_b_t, axi_in_id_t, axi_out_user_t)
`AXI_TYPEDEF_AR_CHAN_T(axi_in_ar_t, axi_addr_t, axi_in_id_t, axi_in_user_t)
`AXI_TYPEDEF_R_CHAN_T(axi_in_r_t, axi_in_data_t, axi_in_id_t, axi_out_user_t)
`AXI_TYPEDEF_REQ_T(axi_in_req_t, axi_in_aw_t, axi_in_w_t, axi_in_ar_t)
`AXI_TYPEDEF_RESP_T(axi_in_rsp_t, axi_in_b_t, axi_in_r_t)

`AXI_TYPEDEF_AW_CHAN_T(axi_out_aw_t, axi_addr_t, axi_out_id_t, axi_out_user_t)
`AXI_TYPEDEF_W_CHAN_T(axi_out_w_t, axi_out_data_t, axi_out_strb_t, axi_out_user_t)
`AXI_TYPEDEF_B_CHAN_T(axi_out_b_t, axi_out_id_t, axi_out_user_t)
`AXI_TYPEDEF_AR_CHAN_T(axi_out_ar_t, axi_addr_t, axi_out_id_t, axi_out_user_t)
`AXI_TYPEDEF_R_CHAN_T(axi_out_r_t, axi_out_data_t, axi_out_id_t, axi_out_user_t)
`AXI_TYPEDEF_REQ_T(axi_out_req_t, axi_out_aw_t, axi_out_w_t, axi_out_ar_t)
`AXI_TYPEDEF_RESP_T(axi_out_rsp_t, axi_out_b_t, axi_out_r_t)


// -----------------
//  Clock and reset
// -----------------
  logic clk;
  logic rst_n;

  initial begin
    rst_n = 0;
    repeat (3) begin
      #(ClkPeriod/2) clk = 0;
      #(ClkPeriod/2) clk = 1;
    end
    rst_n = 1;
    forever begin
      #(ClkPeriod/2) clk = 0;
      #(ClkPeriod/2) clk = 1;
    end
  end

// -------------------
//  Bus and Interfaces
// -------------------
  // Core-side LSU Interface
  LSU_STREAM_BUS_DV #(
      .AW(AddrWidth),
      .DW(DataWidth_I)
    ) lsu_bus_dv(clk);

  //ssr axi bus
  AXI_BUS_DV #(
    .AXI_ADDR_WIDTH ( AddrWidth ),
    .AXI_DATA_WIDTH ( DataWidth_I ),
    .AXI_ID_WIDTH   ( AxiIdWidth_I  ),
    .AXI_USER_WIDTH ( AxiUserWidth_I )
  ) ssr_axi_bus_dv (clk);

  //Bus to axi slave
  AXI_BUS_DV #(
    .AXI_ADDR_WIDTH ( AddrWidth ),
    .AXI_DATA_WIDTH ( DataWidth_O ),
    .AXI_ID_WIDTH   ( AxiIdWidth_O  ),
    .AXI_USER_WIDTH ( AxiUserWidth_O )
  ) axi_slave_bus_dv (clk);

  lsu_req_t lsu_req;
  lsu_rsp_t lsu_rsp;

  `LSU_ASSIGN_INTF_TO_REQRESP(lsu_bus_dv,lsu_req,lsu_rsp)

  
  axi_in_req_t axi_ssr_req;
  axi_in_rsp_t axi_ssr_rsp;
  `AXI_ASSIGN_FROM_REQ(ssr_axi_bus_dv,axi_ssr_req)
  `AXI_ASSIGN_FROM_RESP(ssr_axi_bus_dv, axi_ssr_rsp)

  //ssr axi delay
  axi_in_req_t axi_ssr_delay_req;
  axi_in_rsp_t axi_ssr_delay_rsp;

  //std axi delay
  axi_out_req_t axi_delay_req;
  axi_out_rsp_t axi_delay_rsp;

  //axi reqrsp to aix_memory
  axi_out_req_t axi_req;
  axi_out_rsp_t axi_rsp;

  `AXI_ASSIGN_FROM_REQ(axi_slave_bus_dv,axi_req)
  `AXI_ASSIGN_FROM_RESP(axi_slave_bus_dv, axi_rsp)

// -----
//  DUT
// -----

// Snitch LSU
  snitch_lsu_ssr #(
    .lsu_req_t (lsu_req_t),
    .lsu_rsp_t (lsu_rsp_t),
    .axi_req_t (axi_in_req_t),
    .axi_rsp_t (axi_in_rsp_t),
    .NumOutstandingStores (NumIntOutstandingStores),
    .NumOutstandingLoads (NumIntOutstandingLoads)
  ) i_snitch_lsu (
    .clk_i (clk),
    .rst_i (~rst_n),
    .lsu_req_i(lsu_req),
    .lsu_rsp_o(lsu_rsp),
    .axi_req_o(axi_ssr_req),
    .axi_rsp_i(axi_ssr_rsp)
  );

//Input delay
axi_delayer #(
  // AXI channel types
  .aw_chan_t(axi_in_aw_t),
  .w_chan_t(axi_in_w_t),
  .b_chan_t(axi_in_b_t),
  .ar_chan_t(axi_in_ar_t),
  .r_chan_t(axi_in_r_t),
  // AXI request & response types
  .req_t(axi_in_req_t),
  .resp_t(axi_in_rsp_t),
  // delay parameters
  .StallRandomInput(0),
  .StallRandomOutput(0),
  .FixedDelayInput(InputDelay),
  .FixedDelayOutput(InputDelay)
) i_input_delay(
  .clk_i(clk),      // Clock
  .rst_ni(rst_n),     // Asynchronous reset active low
  // slave port
  .slv_req_i(axi_ssr_req),
  .slv_resp_o(axi_ssr_rsp),
  // master port
  .mst_req_o(axi_ssr_delay_req),
  .mst_resp_i(axi_ssr_delay_rsp)
);

//Filter
  axi_pack_filter_muxed #(
    .AddrWidth(AddrWidth),
    .DataWidth_I(DataWidth_I),
    .DataWidth_O(DataWidth_O),
    .AxiIdWidth(AxiIdWidth_I),
    .axi_ssr_req_t      (axi_in_req_t),
    .axi_ssr_rsp_t      (axi_in_rsp_t),
    .axi_req_t (axi_out_req_t),
    .axi_rsp_t (axi_out_rsp_t),
    .axi_ssr_aw_chan_t(axi_in_aw_t),
    .axi_ssr_ar_chan_t(axi_in_ar_t),
    .axi_ssr_w_chan_t(axi_in_w_t),
    .axi_ssr_r_chan_t(axi_in_r_t),
    .axi_ssr_b_chan_t(axi_in_b_t),
    .axi_aw_chan_t(axi_out_aw_t),
    .axi_ar_chan_t(axi_out_ar_t),
    .axi_w_chan_t(axi_out_w_t),
    .axi_r_chan_t(axi_out_r_t),
    .axi_b_chan_t(axi_out_b_t)
)i_filter(
    .clk_i (clk),
    .rst_i (~rst_n),
    .axi_ssr_req_i(axi_ssr_delay_req),
    .axi_ssr_rsp_o(axi_ssr_delay_rsp),
    .axi_req_o    (axi_delay_req),
    .axi_rsp_i    (axi_delay_rsp)
);

//Output delay
axi_delayer #(
  // AXI channel types
  .aw_chan_t(axi_out_aw_t),
  .w_chan_t(axi_out_w_t),
  .b_chan_t(axi_out_b_t),
  .ar_chan_t(axi_out_ar_t),
  .r_chan_t(axi_out_r_t),
  // AXI request & response types
  .req_t(axi_out_req_t),
  .resp_t(axi_out_rsp_t),
  // delay parameters
  .StallRandomInput(0),
  .StallRandomOutput(0),
  .FixedDelayInput(OutputDelay),
  .FixedDelayOutput(OutputDelay)
) i_output_delay(
  .clk_i(clk),      // Clock
  .rst_ni(rst_n),     // Asynchronous reset active low
  // slave port
  .slv_req_i(axi_delay_req),
  .slv_resp_o(axi_delay_rsp),
  // master port
  .mst_req_o(axi_req),
  .mst_resp_i(axi_rsp)
);


//axi simulation memory
axi_sim_mem #(
  .AddrWidth          (AddrWidth),
  .DataWidth          (DataWidth_O),
  .IdWidth            (AxiIdWidth_O),
  .UserWidth          (AxiUserWidth_O),
  .req_t              (axi_out_req_t),
  .rsp_t              (axi_out_rsp_t),
  .WarnUninitialized  (1'b0),
  .ApplDelay          (ApplTime),
  .AcqDelay           (TestTime)
) i_sim_mem (
  .clk_i      (clk),
  .rst_ni     (rst_n),
  .axi_req_i  (axi_req),
  .axi_rsp_o  (axi_rsp)
);



// ------------------
// LSU Master
// ------------------

  typedef core_to_lsu_test::lsu_stream_master #(
    // stream bus interface paramaters;
    .AW ( AddrWidth ),
    .DW ( DataWidth_I ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) lsu_driver_t;

  lsu_driver_t lsu_master = new(lsu_bus_dv);

  // LSU master running 
  initial begin
    lsu_master.lsu_reset();
  end

//-------------------
// Monitor
//-------------------
  //LSU master monitor 
  typedef core_to_lsu_test::lsu_stream_monitor #(
    // stream bus interface paramaters;
    .AW ( AddrWidth ),
    .DW ( DataWidth_I ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) lsu_monitor_t;

  lsu_monitor_t lsu_monitor = new(lsu_bus_dv);

  initial begin
    @(posedge rst_n);
    lsu_monitor.monitor();
  end

  // SSR AXI bus Monitor
  typedef axi_test::axi_monitor #(
    // AXI interface parameters
    .AW ( AddrWidth ),
    .DW ( DataWidth_I ),
    .IW ( AxiIdWidth_I ),
    .UW ( AxiUserWidth_I ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) ssr_axi_monitor_t;

  ssr_axi_monitor_t ssr_axi_monitor = new (ssr_axi_bus_dv);
  initial begin
    @(posedge rst_n);
    ssr_axi_monitor.monitor();
  end

  // AXI slave bus Monitor
  typedef axi_test::axi_monitor #(
    // AXI interface parameters
    .AW ( AddrWidth ),
    .DW ( DataWidth_O ),
    .IW ( AxiIdWidth_O ),
    .UW ( AxiUserWidth_O ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) axi_monitor_t;

  axi_monitor_t axi_monitor = new (axi_slave_bus_dv);
  initial begin
    @(posedge rst_n);
    axi_monitor.monitor();
  end

//******************
//Simulatiuon Tasks
//******************

task automatic testRandStream(int num_test);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) l = new;
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) load_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) store_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) req_end_mbx[$];

  automatic real total_write_beat_transfer=0, total_write_cycles=0;
  automatic real total_read_beat_transfer=0, total_read_cycles=0;
  automatic real total_beat_transfer=0, total_cycles=0;
  automatic real total_write_byte_transfer=0,total_read_byte_transfer=0;
  automatic real total_byte_transfer=0;
  time entire_start_time,entire_end_time;
 
  entire_start_time = $time;
  fork
    //request
    for (int i = 0; i < num_test; i++) begin
      q.randomize() with {
                      q_ssr_user.indirect_enable == 0;
                      // q_write == 1;
                      q_size == 2;
                      };
      // assert(q.randomize());
      // if (i%2) begin
      //   q.randomize() with {q_ssr_user.indirect_enable == 1;q_write == 0;};
      // end else begin
      //   q.randomize() with {q_ssr_user.indirect_enable == 1;q_write == 1;};
      // end
      $display("***********************************");
      $displayh("Stream request = %p",q);
      if (q.q_write) begin
        automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q_s = new q;
        //q_s.copy(q);
        // $displayh("Stream request = %p",q);
        store_queue.push_back(q_s);
      end else begin
        automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q_l = new q; 
        //q_l.copy(q);
        load_queue.push_back(q_l);
      end
      lsu_master.send_lsu_req(q);
      if (i == num_test-1) begin
        req_end_mbx.push_back(q);
      end
    end

    //Store
    while((store_queue.size() != 0) || (req_end_mbx.size() ==0 )) begin
      if (store_queue.size() != 0) begin
        int total_len;
        time start_time, end_time;
        total_len= store_queue[0].q_len;
        start_time = $time;
        for (int i = 0; i < total_len+1 ; i++) begin
          assert(s.randomize());
          s.s_last = (i == total_len) ? 1:0 ;
          lsu_master.send_store_data(s);
          // $display("Write transfer: %d/%d, last = %d",i+1,total_len+1,s.s_last);
        end
        end_time = $time;
        // $display("total_len=%d, period = %d",total_len+1,(end_time-start_time)/ClkPeriod);
        total_write_byte_transfer+=( {1'b0,total_len}+1)*(2**store_queue[0].q_size);
        total_write_beat_transfer+= {1'b0,total_len}+1;
        total_write_cycles += (end_time-start_time)/ClkPeriod;
        store_queue.pop_front();
      end else begin
        lsu_master.cycle_start();
        lsu_master.cycle_end();
      end
    end

    //Load
    while((load_queue.size() != 0) || (req_end_mbx.size() ==0 )) begin
      if (load_queue.size() != 0) begin
        time start_time, end_time;
        start_time = $time;
        for (int i = 0; i < load_queue[0].q_len+1 ; i++) begin
          lsu_master.recv_load_data(l);
          // $display("read transfer: %d/%d",i+1,load_queue[0].q_len+1);
        end
        end_time = $time;
        total_read_byte_transfer+= ({1'b0,load_queue[0].q_len}+1)*(2**load_queue[0].q_size);
        total_read_beat_transfer+= {1'b0,load_queue[0].q_len}+1;
        total_read_cycles += (end_time-start_time)/ClkPeriod;
        load_queue.pop_front();
      end else begin
        lsu_master.cycle_start();
        lsu_master.cycle_end();
      end
    end

  join
  entire_end_time = $time;
  total_beat_transfer = total_write_beat_transfer+total_read_beat_transfer;
  total_byte_transfer = total_write_byte_transfer+total_read_byte_transfer;
  total_cycles = (entire_end_time-entire_start_time)/ClkPeriod;

  $display("total write transfer:%d, total write cycles:%d, write latancy:%f, write bus utlization: %f",
          total_write_beat_transfer,
          total_write_cycles,
          (total_write_cycles/total_write_beat_transfer)-1,
          (100*total_write_beat_transfer/total_cycles));
  $display("total read transfer:%d, total read cycles:%d, read latancy:%f, write bus utlization: %f",
          total_read_beat_transfer,
          total_read_cycles,
          (total_read_cycles/total_read_beat_transfer)-1,
          (100*total_read_beat_transfer/total_cycles));
  $display("total byte transfer:%d, total  cycles:%d,",
          total_byte_transfer,
          total_cycles);
endtask 

task sim_filter();
  automatic int max_test=300;
  testRandStream(max_test);
endtask 

//*****
//Test
//*****

  initial begin
    @(posedge rst_n);
    sim_filter();
    $finish;
  end


endmodule