//Author: Chi Zhang <chizhang@student.ethz.ch>
`include "reqrsp_interface/typedef.svh"
`include "reqrsp_interface/assign.svh"
`include "axi/assign.svh"
`include "axi/typedef.svh"
`include "axi_ssr/typedef.svh"
`include "axi_ssr/assign.svh"

`timescale 1ns/1ns

module tb_ssr_lsu_and_filter_simulation;
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

localparam int unsigned NumIntOutstandingStores = 5;
localparam int unsigned NumIntOutstandingLoads = 5;
localparam bit          AxiMemWarnUninitialized = 1'b0;


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
    .axi_ssr_req_i(axi_ssr_req),
    .axi_ssr_rsp_o(axi_ssr_rsp),
    .axi_req_o    (axi_req),
    .axi_rsp_i    (axi_rsp)
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

// ----------
// Scoreboard
// ----------

task automatic clearMonitorData();
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) store;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) load = new;
  automatic axi_monitor_t::ax_beat_t aw;
  automatic axi_monitor_t::w_beat_t w;
  automatic axi_monitor_t::ax_beat_t ar;
  automatic axi_monitor_t::r_beat_t r;

  while (lsu_monitor.req_mbx.num()) begin
    lsu_monitor.req_mbx.get(lsu_req);
  end

  while (lsu_monitor.store_mbx.num()) begin
    lsu_monitor.store_mbx.get(store);
  end

  while (lsu_monitor.load_mbx.num()) begin
    lsu_monitor.load_mbx.get(load);
  end

  while (axi_monitor.aw_mbx.num()) begin
    axi_monitor.aw_mbx.get(aw);
  end

  while (axi_monitor.w_mbx.num()) begin
    axi_monitor.w_mbx.get(w);
  end

  while (axi_monitor.ar_mbx.num()) begin
    axi_monitor.ar_mbx.get(ar);
  end

  while (axi_monitor.r_mbx.num()) begin
    axi_monitor.r_mbx.get(r);
  end

endtask 

task automatic scoreboard_filter_indirect_read(addr_t index_mem_start_addr, int index_bytes,int num_bytes, ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) load = new;
  automatic logic [7:0] ref_mem[] = new [num_bytes];
  ref_mem = mem;

  while (lsu_monitor.req_mbx.num()) begin
    axi_pack_pkg::indirect_t indirect;
    int index_base_addr,index_base_offset;
    lsu_monitor.req_mbx.get(lsu_req);
    assert(lsu_req.q_ssr_user.indirect_enable==1) else $fatal("in scoreboard_filter_indirect_read, only allow indirect");
    assert(lsu_req.q_write==0) else $fatal("in scoreboard_filter_indirect_read, only allow read transfer");
    indirect = lsu_req.q_ssr_user.user.indirect;
    assert(indirect.index_size == 0) else $fatal("in scoreboard_filter_indirect_read, now only allow index size = 0 ");
    index_base_offset = $signed(indirect.index_base_offset);
    $display("index_base_offset:%d",index_base_offset);
    index_base_addr = lsu_req.q_addr + index_base_offset;
    for (int i = 0; i < lsu_req.q_len+1; i++) begin
      automatic int index, read_addr, index_base;
      lsu_monitor.load_mbx.get(load);
      index = ref_mem [index_base_addr + i - index_mem_start_addr];
      read_addr = lsu_req.q_addr + index*(2**lsu_req.q_size);
      index_base = read_addr;
      if ((index_base+2**lsu_req.q_size)<=num_bytes && index_base>=0) begin 
        // $display("Indirect read Nr%d, index = %d, size = %h, data = %h, ref = %h",i,index,lsu_req.q_size,load.l_data, {ref_mem[index_base+3],ref_mem[index_base+2],ref_mem[index_base+1],ref_mem[index_base]});
        case (lsu_req.q_size)
          0: assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
          1: begin
            assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
            assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
          end
          2: begin
            assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
            assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
            assert(ref_mem[index_base+2] == load.l_data[23:16]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+2,ref_mem[index_base+2],load.l_data[23:16]);
            assert(ref_mem[index_base+3] == load.l_data[31:24]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+3,ref_mem[index_base+3],load.l_data[31:24]);
          end
          default : assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
        endcase
      end
    end
  end

  clearMonitorData();

endtask 

task automatic scoreboard_filter_indirect_write(addr_t index_mem_start_addr, int num_bytes, ref logic [7:0] mem[], output int error);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) store;
  automatic axi_monitor_t::ax_beat_t aw;
  automatic axi_monitor_t::w_beat_t w;
  automatic logic [7:0] index_mem[] = new [num_bytes];
  index_mem = mem;

  while (lsu_monitor.req_mbx.num()) begin
    axi_pack_pkg::indirect_t indirect;
    int index_base_addr,index_base_offset;
    int any_error;
    lsu_monitor.req_mbx.get(lsu_req);
    assert(lsu_req.q_ssr_user.indirect_enable==1) else $fatal("in scoreboard_filter_indirect_write, only allow indirect");
    assert(lsu_req.q_write==1) else $fatal("in scoreboard_filter_indirect_write, only allow write transfer");
    indirect = lsu_req.q_ssr_user.user.indirect;
    assert(indirect.index_size == 0) else $fatal("in scoreboard_filter_indirect_write, now only allow index size = 0 ");
    index_base_offset = $signed(indirect.index_base_offset);
    $display("index_base_offset:%d",index_base_offset);
    index_base_addr = lsu_req.q_addr + index_base_offset;
    any_error = 0;
    for (int i = 0; i < lsu_req.q_len+1; i++) begin
      // assert(lsu_monitor.store_mbx.num()!=0) else $fatal("Nr %d Master Write lose",i);
      // assert(axi_monitor.aw_mbx.num()!=0) else $fatal("Nr %d Slave AW lose",i);
      // assert(axi_monitor.w_mbx.num()!=0) else $fatal("Nr %d Slave Write lose",i);
      lsu_monitor.store_mbx.get(store);
      axi_monitor.aw_mbx.get(aw);
      axi_monitor.w_mbx.get(w);
      //$display("index_base_addr:%h",index_base_addr);
      if ((index_base_addr + i - index_mem_start_addr)<num_bytes) begin
        automatic int index, write_addr, axi_offset, offset_w_data;
        index = index_mem [index_base_addr + i - index_mem_start_addr];
        write_addr = lsu_req.q_addr + index*(2**lsu_req.q_size);
        assert (aw.ax_addr == write_addr) else begin
          $error("Nr %d Incorrect addr! ax addr:%h, write_addr:%h, index:%d",i,aw.ax_addr,write_addr,index);
          any_error = 1;
        end 
        axi_offset = write_addr[DataAlign_I-1:0];
        offset_w_data = w.w_data>>{axi_offset,3'b0};
        assert (store.s_data[7:0] == offset_w_data[7:0]) else begin
          $error("Nr %d Incorrect data! axi_offset:%d, store.s_data:%h, w.w_data:%h",i,axi_offset,store.s_data,w.w_data);
          any_error = 1;
        end 
      end 
    end

    if (any_error) begin
      $display("Some errors occurs, Please check it");
    end else $display("Indirect Write Check Correct! Good job");

    error = any_error;
  end


  clearMonitorData();

endtask 

task scoreboard_filter_affine_write(int max_write_addr = 2**12);
  automatic int mem_num = max_write_addr;
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) store;
  automatic axi_monitor_t::ax_beat_t aw;
  automatic axi_monitor_t::w_beat_t w;
  logic [7:0] mem_ssr[],mem_slave[];
  mem_ssr = new [mem_num];
  mem_slave = new [mem_num];
  begin
    for (int i = 0; i < mem_num; i++) begin
      mem_ssr[i] = 8'b0;
      mem_slave[i] = 8'b0;
    end
  end
  while (lsu_monitor.req_mbx.num()) begin
    int n0,n1,n1_remain,n2,s0,s1,s2,addr;
    axi_pack_pkg::affine_t affine;
    lsu_monitor.req_mbx.get(lsu_req);
    assert(lsu_req.q_ssr_user.indirect_enable==0) else $fatal("in scoreboard_filter_affine_write, only allow affine");
    assert(lsu_req.q_write==1) else $fatal("in scoreboard_filter_affine_write, only allow write transfer");
    //$displayh("Master send req = %p", lsu_req);
    affine = lsu_req.q_ssr_user.user.affine;
    addr = lsu_req.q_addr;
    n1 = ((lsu_req.q_len+1)/(affine.nest_len+1));
    n1_remain = ((lsu_req.q_len+1)%(affine.nest_len+1));
    n0 = affine.nest_len+1;
    n2 = 1;
    s1 = affine.stride+1;
    s0 = affine.nest_stride+1;
    s2 = 1;
    for (int i = 0; i < n2; i++) begin
      for (int j = 0; j < n1; j++) begin
        for (int k = 0; k < n0; k++) begin
          lsu_monitor.store_mbx.get(store);
          //$display("Master send data in addr: %h, size: %d data: %h",addr+s0*k+s1*j+s2*i,lsu_req.q_size,store.s_data);
          if ((addr+(s0*k+s1*j+s2*i+1)*(2**lsu_req.q_size))<=mem_num) begin
            case (lsu_req.q_size)
              0: mem_ssr[addr+s0*k+s1*j+s2*i] = store.s_data[7:0];
              1: begin
                mem_ssr[addr+(s0*k+s1*j+s2*i)*2] = store.s_data[7:0];
                mem_ssr[addr+(s0*k+s1*j+s2*i)*2+1] = store.s_data[15:8];
              end
              2: begin
                mem_ssr[addr+(s0*k+s1*j+s2*i)*4] = store.s_data[7:0];
                mem_ssr[addr+(s0*k+s1*j+s2*i)*4+1] = store.s_data[15:8];
                mem_ssr[addr+(s0*k+s1*j+s2*i)*4+2] = store.s_data[23:16];
                mem_ssr[addr+(s0*k+s1*j+s2*i)*4+3] = store.s_data[31:24];
              end
              default : mem_ssr[addr+s0*k+s1*j+s2*i] = store.s_data[7:0];
            endcase
          end  
        end
      end
    end
    for (int k = 0; k < n1_remain; k++) begin
      int i, j;
      i = 0;
      j=n1;
      lsu_monitor.store_mbx.get(store);
      //$display("Master send data in addr: %h, size: %d data: %h",addr+s0*k+s1*j+s2*i,lsu_req.q_size,store.s_data);
      if ((addr+(s0*k+s1*j+s2*i+1)*(2**lsu_req.q_size))<=mem_num) begin
        case (lsu_req.q_size)
          0: mem_ssr[addr+s0*k+s1*j+s2*i] = store.s_data[7:0];
          1: begin
            mem_ssr[addr+(s0*k+s1*j+s2*i)*2] = store.s_data[7:0];
            mem_ssr[addr+(s0*k+s1*j+s2*i)*2+1] = store.s_data[15:8];
          end
          2: begin
            mem_ssr[addr+(s0*k+s1*j+s2*i)*4] = store.s_data[7:0];
            mem_ssr[addr+(s0*k+s1*j+s2*i)*4+1] = store.s_data[15:8];
            mem_ssr[addr+(s0*k+s1*j+s2*i)*4+2] = store.s_data[23:16];
            mem_ssr[addr+(s0*k+s1*j+s2*i)*4+3] = store.s_data[31:24];
          end
          default : mem_ssr[addr+s0*k+s1*j+s2*i] = store.s_data[7:0];
        endcase
      end  
    end
  end
  //$displayh("ssr mem = %p",mem_ssr);

  while (axi_monitor.aw_mbx.num()) begin
    automatic logic[AddrWidth-1:0] addr;
    axi_monitor.aw_mbx.get(aw);
    //$displayh("Slave receive req = %p", aw);
    addr = aw.ax_addr;
    for (int i = 0; i < aw.ax_len+1; i++) begin
      automatic logic [1:0] offset;
      automatic logic [AddrWidth-1:0] start_addr;
      automatic logic [DataWidth_O-1:0] shifted_data;
      automatic logic [3:0]           shifted_strb;
      axi_monitor.w_mbx.get(w);
      //$displayh("Index:%d, Slave receive data = %p", i,w);
      start_addr = addr+i*(2**aw.ax_size);
      offset = start_addr[1:0];
      shifted_data = w.w_data>>{offset, 3'b000};
      shifted_strb = w.w_strb>>offset;
      //$displayh("start addr = %h, shifted_data = %h, shifted_strb = %h", start_addr,shifted_data,shifted_strb);
      if ((start_addr+(2**aw.ax_size)) <= mem_num) begin
        case (aw.ax_size)
          0: begin 
            if (shifted_strb[0]) mem_slave[start_addr] =  shifted_data[7:0];
          end
          1: begin 
            if (shifted_strb[0]) mem_slave[start_addr] =  shifted_data[7:0];
            if (shifted_strb[1]) mem_slave[start_addr+1] =  shifted_data[15:8];
          end
          2: begin 
            if (shifted_strb[0]) mem_slave[start_addr] =  shifted_data[7:0];
            if (shifted_strb[1]) mem_slave[start_addr+1] =  shifted_data[15:8];
            if (shifted_strb[2]) mem_slave[start_addr+2] =  shifted_data[23:16];
            if (shifted_strb[3]) mem_slave[start_addr+3] =  shifted_data[31:24];
          end
          default : if (shifted_strb[0]) mem_slave[start_addr] =  shifted_data[7:0];
        endcase
      end
    end
  end

  if (mem_ssr!=mem_slave) begin
    for (int i = 0; i < mem_num; i++) begin
      if (mem_ssr[i] != mem_slave[i]) begin
        $display("Error i=%d,mem_ssr[i]=%h,mem_slave[i]=%h",i,mem_ssr[i],mem_slave[i]);
      end
    end
    assert (mem_ssr == mem_slave);
    $displayh("slave mem = %p",mem_slave);
    $displayh("ssr   mem = %p",mem_ssr);
  end else begin
    $display("Write Data Check Correct!, Good job",);
  end

  clearMonitorData();

endtask 

task automatic scoreboard_filter_affine_read(addr_t start_addr, int num_bytes, ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) lsu_req;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) load = new;
  automatic logic [7:0] ref_mem[] = new [num_bytes];
  ref_mem = mem;
  while (lsu_monitor.req_mbx.num()) begin
    int n0,n1,n1_remain,n2,s0,s1,s2,addr;
    axi_pack_pkg::affine_t affine;
    lsu_monitor.req_mbx.get(lsu_req);
    assert(lsu_req.q_ssr_user.indirect_enable==0) else $fatal("in scoreboard_filter_affine_read, only allow affine");
    assert(lsu_req.q_write==0) else $fatal("in scoreboard_filter_affine_read, only allow read transfer");
    //$displayh("Master send req = %p", lsu_req);
    affine = lsu_req.q_ssr_user.user.affine;
    addr = lsu_req.q_addr;
    n1 = ((lsu_req.q_len+1)/(affine.nest_len+1));
    n1_remain = ((lsu_req.q_len+1)%(affine.nest_len+1));
    n0 = affine.nest_len+1;
    n2 = 1;
    s1 = affine.stride+1;
    s0 = affine.nest_stride+1;
    s2 = 1;
    for (int i = 0; i < n2; i++) begin
      for (int j = 0; j < n1; j++) begin
        for (int k = 0; k < n0; k++) begin
          int index_base = addr+(s0*k+s1*j+s2*i)*(2**lsu_req.q_size)-start_addr;
          //$display("index_base = %h",index_base);
          lsu_monitor.load_mbx.get(load);
          //$display("Master send data in addr: %h, size: %d data: %h",addr+s0*k+s1*j+s2*i,lsu_req.q_size,store.s_data);
          if ((index_base+2**lsu_req.q_size)<=num_bytes && index_base>=0) begin
            case (lsu_req.q_size)
              0: assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
              1: begin
                assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
                assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
              end
              2: begin
                assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
                assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
                assert(ref_mem[index_base+2] == load.l_data[23:16]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+2,ref_mem[index_base+2],load.l_data[23:16]);
                assert(ref_mem[index_base+3] == load.l_data[31:24]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+3,ref_mem[index_base+3],load.l_data[31:24]);
              end
              default : assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
            endcase
          end  
        end
      end
    end
    for (int k = 0; k < n1_remain; k++) begin
      int i = 0, j = n1;
      int index_base = addr+(s0*k+s1*j+s2*i)*(2**lsu_req.q_size)-start_addr;
      //$display("index_base = %h",index_base);
      lsu_monitor.load_mbx.get(load);
      //$display("Master send data in addr: %h, size: %d data: %h",addr+s0*k+s1*j+s2*i,lsu_req.q_size,store.s_data);
      if ((index_base+2**lsu_req.q_size)<=num_bytes && index_base>=0) begin
        case (lsu_req.q_size)
          0: assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
          1: begin
            assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
            assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
          end
          2: begin
            assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
            assert(ref_mem[index_base+1] == load.l_data[15:8]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+1,ref_mem[index_base+1],load.l_data[15:8]);
            assert(ref_mem[index_base+2] == load.l_data[23:16]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+2,ref_mem[index_base+2],load.l_data[23:16]);
            assert(ref_mem[index_base+3] == load.l_data[31:24]) else $error("Incorrect index:%h ref:%h, load:%h",index_base+3,ref_mem[index_base+3],load.l_data[31:24]);
          end
          default : assert(ref_mem[index_base] == load.l_data[7:0]) else $error("Incorrect index:%h ref:%h, load:%h",index_base,ref_mem[index_base],load.l_data[7:0]);
        endcase
      end  
    end
  end

  clearMonitorData();
 
endtask 



//******************
//Simulatiuon Tasks
//******************

task initMemSpaceRand(addr_t start_addr, int num_bytes, output logic [7:0] mem[] );
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  automatic int mem_cnt = 0;
  mem = new [num_bytes];
  //Regular transfer
  for (int i = 0; i < (num_bytes/256); i++) begin
    $display("sending1");
    q.randomize() with {q_len == 255;
                      q_size == 0; 
                      q_write == 1;
                      q_ssr_user.indirect_enable == 0;
                      q_ssr_user.user.affine.stride == 0;
                      q_ssr_user.user.affine.nest_len == 0;
                      q_ssr_user.user.affine.nest_stride == 0;
                      };
    q.q_addr = start_addr + 256*i;
    lsu_master.send_lsu_req(q);
    for (int j = 0; j < 255; j++) begin
      s.randomize() with {s_data < 10; };
      s.s_last = '0;
      lsu_master.send_store_data(s);
      mem[mem_cnt] = s.s_data[7:0];
      mem_cnt++;
    end
    s.randomize() with {s_data < 10; };
    s.s_last = 1;
    lsu_master.send_store_data(s);
    mem[mem_cnt] = s.s_data[7:0];
    mem_cnt++;
  end
  //last transfer
  q.randomize() with {q_len == ((num_bytes%256) -1);
                      q_size == 0; 
                      q_write == 1;
                      q_ssr_user.indirect_enable == 0;
                      q_ssr_user.user.affine.stride == 0;
                      q_ssr_user.user.affine.nest_len == 0;
                      q_ssr_user.user.affine.nest_stride == 0;
                      };
  q.q_addr = start_addr + 256*(num_bytes/256);
  lsu_master.send_lsu_req(q);
  for (int j = 0; j < ((num_bytes%256) -1); j++) begin
    $display("sending2");
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

task automatic testAffineRead(int num_bytes);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) l = new;
  q.randomize() with {q_addr == 0; 
                      q_ssr_user.indirect_enable == 0;
                      (q_len+1)*(q_ssr_user.user.affine.stride+1)*(2**q_size)<num_bytes || (q_len+1)*(q_ssr_user.user.affine.nest_stride+1)*(2**q_size)<num_bytes; 
                      q_write == 0;
                      };
  $displayh("Stream request = %p",q);
  lsu_master.send_lsu_req(q);
  for (int i = 0; i < q.q_len+1; i++) begin
    lsu_master.recv_load_data(l);
    //$displayh("Master recieve data: %p",l);
  end

endtask 

task automatic testAffineWrite(int start_addr,int num_bytes);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  q.randomize() with {q_addr > start_addr;  
                      q_addr < start_addr+num_bytes-4; 
                      q_len < ((start_addr+num_bytes-q_addr-4)>>q_size); 
                      q_write == 1;
                      q_ssr_user.indirect_enable == 0;
                      };
  lsu_master.send_lsu_req(q);
  $displayh("Stream request = %p",q);
  for (int i = 0; i < q.q_len+1; i++) begin
    assert(s.randomize());
    s.s_last = (i == q.q_len) ? 1:0 ;
    lsu_master.send_store_data(s);
  end
endtask 


task automatic writeIndexArray(int start_addr, int num_bytes,int size,ref logic [7:0] mem[]);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  automatic logic [7:0] write_mem[] = new [num_bytes];
  write_mem = mem;
  q.q_addr = start_addr;
  q.q_write = 1;
  q.q_size = size;
  q.q_len = num_bytes-1;
  q.q_ssr_user = '0;
  $displayh("Stream request = %p",q);
  lsu_master.send_lsu_req(q);
  for (int i = 0; i < q.q_len+1; i++) begin
    s.s_data = write_mem[i];
    s.s_last = (i == q.q_len) ? 1:0 ;
    lsu_master.send_store_data(s);
  end
endtask  


task automatic testIndirectWrite(int index_start_addr,int index_end_addr,int index_size, int num_bytes);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  automatic int signed index_base_offset;
  q.randomize() with {q_addr > index_end_addr; 
                      q_addr < index_end_addr+2**12;
                      q_ssr_user.indirect_enable == 1;
                      q_len < num_bytes;
                      q_write == 1;
                      };
  index_base_offset = index_start_addr - q.q_addr;
  $display("index_base_offset = %h",index_base_offset);
  q.q_ssr_user.user.indirect.index_size = index_size;
  q.q_ssr_user.user.indirect.index_base_offset = index_base_offset;
  $displayh("Stream request = %p",q);
  lsu_master.send_lsu_req(q);
  for (int i = 0; i < q.q_len+1; i++) begin
    assert(s.randomize());
    s.s_last = (i == q.q_len) ? 1:0 ;
    lsu_master.send_store_data(s);
  end

  // lsu_master.cycle_start();
  // lsu_master.cycle_end();

endtask 

task automatic testIndirectRead(int index_start_addr,int index_end_addr,int read_start_addr, int read_end_addr,int index_size, int num_bytes);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) l = new;
  automatic int signed index_base_offset;
  q.randomize() with {q_addr > read_start_addr; 
                      q_addr < read_end_addr;
                      q_ssr_user.indirect_enable == 1;
                      q_len < num_bytes;
                      q_write == 0;
                      };
  index_base_offset = index_start_addr - q.q_addr;
  $display("index_base_offset = %h",index_base_offset);
  q.q_ssr_user.user.indirect.index_size = index_size;
  q.q_ssr_user.user.indirect.index_base_offset = index_base_offset;
  $displayh("Stream request = %p",q);
  lsu_master.send_lsu_req(q);
  for (int i = 0; i < q.q_len+1; i++) begin
    lsu_master.recv_load_data(l);
  end

  // lsu_master.cycle_start();
  // lsu_master.cycle_end();

endtask 

task automatic testRandStream(int num_test);
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q = new;
  automatic core_to_lsu_test::lsu_store_t #(DataWidth_I) s = new;
  automatic core_to_lsu_test::lsu_load_t #(DataWidth_I) l = new;
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) load_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) store_queue[$];
  automatic core_to_lsu_test::lsu_req_t #(AddrWidth) req_end_mbx[$];



  fork
    //request
    for (int i = 0; i < num_test; i++) begin
      q.randomize() with {
                      q_ssr_user.indirect_enable == 0;
                      // q_write == 0;
                      };
      // assert(q.randomize());
      $display("***********************************");
      // $displayh("Stream request = %p",q);
      if (q.q_write) begin
        automatic core_to_lsu_test::lsu_req_t #(AddrWidth) q_s = new q;
        //q_s.copy(q);
        $displayh("Stream request = %p",q);
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
        total_len= store_queue[0].q_len;
        $display("total_len=%h",total_len);
        for (int i = 0; i < total_len+1 ; i++) begin
          assert(s.randomize());
          s.s_last = (i == total_len) ? 1:0 ;
          lsu_master.send_store_data(s);
          // $display("Write transfer: %d/%d, last = %d",i+1,total_len+1,s.s_last);
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
          // $display("read transfer: %d/%d",i+1,load_queue[0].q_len+1);
        end
        load_queue.pop_front();
      end else begin
        lsu_master.cycle_start();
        lsu_master.cycle_end();
      end
    end

  join
endtask 

task sim_filter();
  automatic int initial_space_bytes = 511,max_test=300,max_write_addr = 2**11;
  automatic logic [7:0] mem[];
  initMemSpaceRand(0,initial_space_bytes,mem);
  // scoreboard_filter_affine_write(max_write_addr);
  $displayh("Time:%0tps,Initialization done Mem = %p",$time,mem);
  $displayh("Time:%0tps,start testIndirectRead************************************************************",$time);
  // for (int i = 0; i < max_test; i++) begin
  //   testIndirectRead( 0, 250, 256,  510, 0, 60);
  //   scoreboard_filter_indirect_read( 0,  250, initial_space_bytes,mem);
  // end
  // $displayh("Time:%0tps,start testIndirectWrite************************************************************",$time);
  // for (int i = 0; i < max_test; i++) begin
  //   int error;
  //   bit [7:0] start_addr;
  //   std::randomize(start_addr) with {start_addr>= 0;start_addr < 100;};
  //   $display("Time:%0tps,index start addr:%d",$time,start_addr);
  //   testIndirectWrite(start_addr,initial_space_bytes,0,100);
  //   scoreboard_filter_indirect_write(0, 250, mem,error);
  //   assert (error == 0) else $finish;
  // end
  // $displayh("Time:%0tps,start testAffineRead************************************************************",$time);
  // for (int i = 0; i < max_test; i++) begin
  //   testAffineRead(initial_space_bytes);
  //   scoreboard_filter_affine_read(0,initial_space_bytes,mem);
  // end
  // $displayh("Time:%0tps,start testAffineWrite************************************************************",$time);
  // for (int i = 0; i < max_test; i++) begin
  //   $display("Test:%d********************************",i);
  //   testAffineWrite(0,max_write_addr);
  //   scoreboard_filter_affine_write(max_write_addr);
  // end
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