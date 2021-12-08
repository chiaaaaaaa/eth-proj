//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "reqrsp_interface/typedef.svh"
`include "reqrsp_interface/assign.svh"
`include "axi/assign.svh"
`include "axi/typedef.svh"
`include "axi_ssr/typedef.svh"
`include "axi_ssr/assign.svh"

`define USE_AXI_SIM_MEM

`timescale 1ns/1ns

module tb_snitch_lsu_ssr;
// ------------
//  Parameters
// ------------

//clock parameter
localparam time ClkPeriod = 10ns;
localparam time ApplTime =  0ns;
localparam time TestTime =  10ns;

//Set up snitch lsu parameters
localparam int unsigned AddrWidth = 32;
localparam int unsigned DataWidth = 32;
localparam int unsigned AxiUserWidth = $bits(axi_pack_pkg::ssr_user_t);
localparam int unsigned AxiIdWidth = 2;
localparam int unsigned AxiStrbWidth = DataWidth / 8;
localparam int unsigned NumIntOutstandingStores = 8;
localparam int unsigned NumIntOutstandingLoads = 8;
localparam bit          AxiMemWarnUninitialized = 1'b0;


//define types 
typedef  logic[4:0] tag_t;
typedef  logic[AddrWidth-1:0] addr_t; 
typedef  logic[DataWidth-1:0] data_t; 
`LSU_TYPEDEF_ALL(lsu, addr_t, data_t)

typedef logic [AddrWidth-1:0] axi_addr_t;
typedef logic [DataWidth-1:0] axi_data_t;
typedef logic [AxiIdWidth-1:0]   axi_id_t;
typedef logic [AxiStrbWidth-1:0] axi_strb_t;
typedef logic [AxiUserWidth-1:0] axi_user_t;
`AXI_TYPEDEF_AW_CHAN_T(axi_aw_t, axi_addr_t, axi_id_t, axi_user_t)
`AXI_TYPEDEF_W_CHAN_T(axi_w_t, axi_data_t, axi_strb_t, axi_user_t)
`AXI_TYPEDEF_B_CHAN_T(axi_b_t, axi_id_t, axi_user_t)
`AXI_TYPEDEF_AR_CHAN_T(axi_ar_t, axi_addr_t, axi_id_t, axi_user_t)
`AXI_TYPEDEF_R_CHAN_T(axi_r_t, axi_data_t, axi_id_t, axi_user_t)
`AXI_TYPEDEF_REQ_T(axi_req_t, axi_aw_t, axi_w_t, axi_ar_t)
`AXI_TYPEDEF_RESP_T(axi_rsp_t, axi_b_t, axi_r_t)


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
      .DW(DataWidth)
    ) lsu_bus_dv(clk);

  //Bus to axi slave
  AXI_BUS_DV #(
    .AXI_ADDR_WIDTH ( AddrWidth ),
    .AXI_DATA_WIDTH ( DataWidth ),
    .AXI_ID_WIDTH   ( AxiIdWidth  ),
    .AXI_USER_WIDTH ( AxiUserWidth )
  ) axi_slave_bus_dv (clk);

  lsu_req_t lsu_req;
  lsu_rsp_t lsu_rsp;

  `LSU_ASSIGN_INTF_TO_REQRESP(lsu_bus_dv,lsu_req,lsu_rsp)

  //axi reqrsp to aix_memory
  axi_req_t axi_req;
  axi_rsp_t axi_rsp;

  `AXI_ASSIGN_FROM_REQ(axi_slave_bus_dv,axi_req)
`ifdef USE_AXI_SIM_MEM
  `AXI_ASSIGN_FROM_RESP(axi_slave_bus_dv, axi_rsp)
`else 
  `AXI_ASSIGN_TO_RESP(axi_rsp,axi_slave_bus_dv)
`endif

// -----
//  DUT
// -----

// Snitch LSU
  snitch_lsu_ssr #(
    .lsu_req_t (lsu_req_t),
    .lsu_rsp_t (lsu_rsp_t),
    .axi_req_t (axi_req_t),
    .axi_rsp_t (axi_rsp_t),
    .NumOutstandingStores (NumIntOutstandingStores),
    .NumOutstandingLoads (NumIntOutstandingLoads)
  ) i_snitch_lsu (
    .clk_i (clk),
    .rst_i (~rst_n),
    .lsu_req_i(lsu_req),
    .lsu_rsp_o(lsu_rsp),
    .axi_req_o(axi_req),
    .axi_rsp_i(axi_rsp)
  );

`ifdef USE_AXI_SIM_MEM
  //axi simulation memory
  axi_sim_mem #(
    .AddrWidth          (AddrWidth),
    .DataWidth          (DataWidth),
    .IdWidth            (AxiIdWidth),
    .UserWidth          (AxiUserWidth),
    .req_t              (axi_req_t),
    .rsp_t              (axi_rsp_t),
    .WarnUninitialized  (1'b0),
    .ApplDelay          (ApplTime),
    .AcqDelay           (TestTime)
  ) i_sim_mem (
    .clk_i      (clk),
    .rst_ni     (rst_n),
    .axi_req_i  (axi_req),
    .axi_rsp_o  (axi_rsp)
  );
`endif


// ------------------
// LSU Master
// ------------------

  typedef core_to_lsu_test::lsu_stream_master #(
    // stream bus interface paramaters;
    .AW ( AddrWidth ),
    .DW ( DataWidth ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) lsu_driver_t;

  lsu_driver_t lsu_master = new(lsu_bus_dv);

  // LSU master running 
  initial begin
    lsu_master.lsu_reset();
  end

// ------------------
// AXI Slave
// ------------------
`ifndef USE_AXI_SIM_MEM
  typedef axi_test::rand_axi_slave #(
    // AXI interface parameters
    .AW ( AddrWidth ),
    .DW ( DataWidth ),
    .IW ( AxiIdWidth ),
    .UW ( AxiUserWidth ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime ),
    .RAND_RESP (1),
    .AX_MIN_WAIT_CYCLES (0),
    .AX_MAX_WAIT_CYCLES (20)
  ) rand_axi_slave_t;

  rand_axi_slave_t rand_axi_slave = new (axi_slave_bus_dv);

  initial begin
    rand_axi_slave.reset();
    @(posedge rst_n);
    rand_axi_slave.run();
  end
`endif

//-------------------
// Monitor
//-------------------
  //LSU master monitor 
  typedef core_to_lsu_test::lsu_stream_monitor #(
    // stream bus interface paramaters;
    .AW ( AddrWidth ),
    .DW ( DataWidth ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) lsu_monitor_t;

  lsu_monitor_t lsu_monitor = new(lsu_bus_dv);

  initial begin
    @(posedge rst_n);
    lsu_monitor.monitor();
  end

  // AXI bus Monitor
  typedef axi_test::axi_monitor #(
    // AXI interface parameters
    .AW ( AddrWidth ),
    .DW ( DataWidth ),
    .IW ( AxiIdWidth ),
    .UW ( AxiUserWidth ),
    // Stimuli application and test time
    .TA ( ApplTime ),
    .TT ( TestTime )
  ) axi_monitor_t;

  axi_monitor_t axi_monitor = new (axi_slave_bus_dv);
  initial begin
    @(posedge rst_n);
    axi_monitor.monitor();
  end

// ----------
// Scoreboard
// ----------

task scoreboard(int display = 0);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth) store;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth) load;
  automatic axi_monitor_t::ax_beat_t ax;
  automatic axi_monitor_t::w_beat_t w;
  automatic axi_monitor_t::b_beat_t b;
  automatic axi_monitor_t::r_beat_t r;
  while (lsu_monitor.req_mbx.num()) begin
    lsu_monitor.req_mbx.get(lsu_req);

    if (lsu_req.q_write) begin
      axi_monitor.aw_mbx.get(ax);
      if (display) begin
        $display("Store Stream************************");
        $displayh("Master send req = %p", lsu_req);
        $displayh("LSU    send AW  = %p", ax);
      end
      assert(lsu_req.q_addr == ax.ax_addr);
      assert(lsu_req.q_len == ax.ax_len);
      assert(lsu_req.q_size == ax.ax_size);
      for (int i = 0; i < lsu_req.q_len+1; i++) begin
        lsu_monitor.store_mbx.get(store);
        axi_monitor.w_mbx.get(w);
        if (display) begin
          $displayh("Master send store = %p", store);
          $displayh("LSU    send W     = %p", w);
        end
      end
      assert(store.s_last == 1);
      assert(w.w_last == 1);
      axi_monitor.b_mbx.get(b);
      if (display) begin
        $display("Slave send a write respond, error = %d",b.b_resp[1]);
      end
    end else begin
      if (display) begin
        $display("Load Stream************************");
        $displayh("Master send req = %p", lsu_req);
      end
      axi_monitor.ar_mbx.get(ax);
      assert(lsu_req.q_addr == ax.ax_addr);
      assert(lsu_req.q_len == ax.ax_len);
      assert(lsu_req.q_size == ax.ax_size);
      for (int i = 0; i < lsu_req.q_len+1; i++) begin
        axi_monitor.r_mbx.get(r);
        lsu_monitor.load_mbx.get(load);
        if (display) begin
          $displayh("LSU    receive R     = %p", r);
          $displayh("Master receive load  = %p", load);
        end
      end
      assert(r.r_last == 1);
      assert(load.l_last == 1);
    end
  end
endtask

//******************
//Simulatiuon Tasks
//******************

task initMemSpaceRand(addr_t start_addr, int num_bytes, output logic [7:0] mem[] );
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth) s = new;
  automatic int mem_cnt = 0;
  mem = new [num_bytes];
  //Regular transfer
  for (int i = 0; i < (num_bytes/256); i++) begin
    assert(q.randomize());
    q.q_write = 1;
    q.q_len = 255;
    q.q_size = 0;
    q.q_addr = start_addr + 256*i;
    lsu_master.send_lsu_req(q);
    for (int j = 0; j < 255; j++) begin
      assert(s.randomize());
      s.s_last = '0;
      lsu_master.send_store_data(s);
      mem[mem_cnt] = s.s_data[7:0];
      mem_cnt++;
    end
    assert(s.randomize());
    s.s_last = 1;
    lsu_master.send_store_data(s);
    mem[mem_cnt] = s.s_data[7:0];
    mem_cnt++;
  end
  //last transfer
  assert(q.randomize());
  q.q_write = 1;
  q.q_len = (num_bytes%256) -1;
  q.q_size = 0;
  q.q_addr = start_addr + 256*(num_bytes/256);
  lsu_master.send_lsu_req(q);
  for (int j = 0; j < ((num_bytes%256) -1); j++) begin
    assert(s.randomize());
    s.s_last = '0;
    lsu_master.send_store_data(s);
    mem[mem_cnt] = s.s_data[7:0];
    mem_cnt++;
  end
  assert(s.randomize());
  s.s_last = 1;
  lsu_master.send_store_data(s);
  mem[mem_cnt] = s.s_data[7:0];
  mem_cnt++;
  
endtask 

task automatic testSameAddrStoreLoad(addr_t start_addr, int num_bytes, ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth) s = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth) l = new;
  q.randomize() with {q_addr > start_addr; 
                      q_addr < start_addr+num_bytes; 
                      q_len < ((start_addr+num_bytes-q_addr)>>q_size); 
                      q_write == 1;};
  $displayh("Stream Same Addr request = %p",q);
  lsu_master.send_lsu_req(q);
  q.q_write = 0;
  lsu_master.send_lsu_req(q);

  begin
    for (int j = 0; j <q.q_len+1; j++) begin
      $display("Nr%d *******************************************",j);
      //Store
      assert(s.randomize());
      s.s_last = (j == q.q_len) ? 1:0 ;
      lsu_master.send_store_data(s);
      $displayh("Store = %p",s);
      case (q.q_size)
        0:mem[q.q_addr-start_addr+j] = s.s_data[7:0];
        1: begin
          mem[q.q_addr-start_addr+2*j] = s.s_data[7:0];
          mem[q.q_addr-start_addr+2*j+1] = s.s_data[15:8];
        end
        2: begin
          mem[q.q_addr-start_addr+4*j] = s.s_data[7:0];
          mem[q.q_addr-start_addr+4*j+1] = s.s_data[15:8];
          mem[q.q_addr-start_addr+4*j+2] = s.s_data[23:16];
          mem[q.q_addr-start_addr+4*j+3] = s.s_data[31:24];
        end
        default : mem[q.q_addr-start_addr+j] = s.s_data[7:0];
      endcase
      //Load
      lsu_master.recv_load_data(l);
      $displayh("Load = %p",l);
      case (q.q_size)
        0: assert(l.l_data[7:0] == s.s_data[7:0]);
        1: assert(l.l_data[15:0] == s.s_data[15:0]);
        2: assert(l.l_data[31:0] == s.s_data[31:0]);
        default : assert(l.l_data == s.s_data);
      endcase
    end
    
  end
endtask 

// a[i+1]=a[i]
task automatic testStreamLadderAddrStoreLoad(int start_addr, int num_bytes, ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth) s = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth) l = new;
  automatic logic [7:0] read_mem[] = new [num_bytes];
  automatic logic [7:0] write_mem[] = new [num_bytes];
  q.randomize() with {q_addr > start_addr; 
                      q_addr < start_addr+num_bytes-4; 
                      q_len < ((start_addr+num_bytes-q_addr-4)>>q_size); 
                      q_len < 16;
                      q_write == 0;};
  $displayh("Stream load Addr request = %p",q);
  lsu_master.send_lsu_req(q);
  q.q_write = 1;
  q.q_addr = q.q_addr + (3'b1 << q.q_size);
  $displayh("Stream Store Addr request = %p",q);
  lsu_master.send_lsu_req(q);
  // $displayh("Mem = %p",mem);
  // $display("Mem[q.q_addr-1] = %h",mem[q.q_addr-1]);
  read_mem = mem;
  write_mem = mem;

  begin
    for (int j = 0; j <q.q_len+1; j++) fork 
      $display("Nr%d *******************************************",j);
       //Store
      begin
        assert(s.randomize());
        s.s_last = (j == q.q_len) ? 1:0 ;
        lsu_master.send_store_data(s);
        $displayh("Store = %p",s);
        case (q.q_size)
          0:write_mem[q.q_addr-start_addr+j] = s.s_data[7:0];
          1: begin
            write_mem[q.q_addr-start_addr+2*j] = s.s_data[7:0];
            write_mem[q.q_addr-start_addr+2*j+1] = s.s_data[15:8];
          end
          2: begin
            write_mem[q.q_addr-start_addr+4*j] = s.s_data[7:0];
            write_mem[q.q_addr-start_addr+4*j+1] = s.s_data[15:8];
            write_mem[q.q_addr-start_addr+4*j+2] = s.s_data[23:16];
            write_mem[q.q_addr-start_addr+4*j+3] = s.s_data[31:24];
          end
          default : write_mem[q.q_addr-start_addr+j] = s.s_data[7:0];
        endcase
      end
      
      //load
      begin
        lsu_master.recv_load_data(l);
        $displayh("Load = %p",l);
        case (q.q_size)
          0:read_mem[q.q_addr-start_addr+j-1] = l.l_data[7:0];
          1: begin
            read_mem[q.q_addr-start_addr+2*(j-1)] = l.l_data[7:0];
            read_mem[q.q_addr-start_addr+2*(j-1)+1] = l.l_data[15:8];
          end
          2: begin
            read_mem[q.q_addr-start_addr+4*(j-1)] = l.l_data[7:0];
            read_mem[q.q_addr-start_addr+4*(j-1)+1] = l.l_data[15:8];
            read_mem[q.q_addr-start_addr+4*(j-1)+2] = l.l_data[23:16];
            read_mem[q.q_addr-start_addr+4*(j-1)+3] = l.l_data[31:24];
          end
          default : read_mem[q.q_addr-start_addr+j-1] = l.l_data[7:0];
        endcase
      end
    join    
  end
  //Check
  for (int i = 0; i < (q.q_len+1)*(2**q.q_size); i++) begin
    $display("check addr = %h",q.q_addr+i-(2**q.q_size));
    assert(read_mem[q.q_addr+i-(2**q.q_size)-start_addr] == write_mem[q.q_addr+i-(2**q.q_size)-start_addr]);
  end
  //Write Back
  mem = write_mem;
endtask 


task automatic testRandStream(int start_addr, int num_bytes, int num_test ,ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth) s = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth) l = new;
  automatic logic [7:0] read_mem[] = new [num_bytes];
  automatic logic [7:0] write_mem[] = new [num_bytes];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) load_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) store_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) req_end_mbx[$];
  read_mem = mem;
  write_mem = mem;

  fork
    //request
    for (int i = 0; i < num_test; i++) begin
      q.randomize() with {q_addr > start_addr; 
                      q_addr < start_addr+num_bytes-4; 
                      q_len < ((start_addr+num_bytes-q_addr-4)>>q_size); 
                      q_len < 30;};
      lsu_master.send_lsu_req(q);
      $displayh("Stream request = %p",q);
      if (q.q_write) begin
        automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q_s = new q;
        //q_s.copy(q);
        store_queue.push_back(q_s);
      end else begin
        automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q_l = new q; 
        //q_l.copy(q);
        load_queue.push_back(q_l);
      end
      if (i == num_test-1) begin
        req_end_mbx.push_back(q);
      end
    end

    //Store
    while((store_queue.size() != 0) || (req_end_mbx.size() ==0 )) begin
      if (store_queue.size() != 0) begin
        for (int i = 0; i < store_queue[0].q_len+1 ; i++) begin
          assert(s.randomize());
          s.s_last = (i == store_queue[0].q_len) ? 1:0 ;
          lsu_master.send_store_data(s);
        end
        store_queue.pop_front();
      end else begin
        lsu_master.cycle_start();
        lsu_master.cycle_end();
      end
    end

    //Load
    while((load_queue.size() != 0) || (req_end_mbx.size() ==0 )) begin
      if (load_queue.size() != 0) begin
        for (int i = 0; i < load_queue[0].q_len+1 ; i++) begin
          lsu_master.recv_load_data(l);
        end
        load_queue.pop_front();
      end else begin
        lsu_master.cycle_start();
        lsu_master.cycle_end();
      end
    end

  join
endtask 

task simulation();
  automatic logic [7:0] mem[];
  initMemSpaceRand(0,280,mem);
  $displayh("Time:%0tps,Initialization done Mem = %p",$time,mem);
  scoreboard();
  for (int i = 0; i < 10; i++) begin
    testStreamLadderAddrStoreLoad(0,280,mem);
  end
  $displayh("Time:%0tps,Ladder stream test done",$time);
  testRandStream(0,280,30,mem);
  $displayh("Time:%0tps,random stream test done",$time);
  $finish;
endtask 


//*****
//Test
//*****

  initial begin
    @(posedge rst_n);
    simulation();
  end



endmodule