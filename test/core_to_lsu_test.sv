package core_to_lsu_test;

import axi_pack_pkg::*;

  class lsu_req_t #(
    parameter int AW = 32
  );
    //TYPE DIFINITION
    typedef  logic[AW-1:0] addr_t; 

    //stream request channel 
    rand logic            q_write;
    rand logic            q_signed;
    rand addr_t           q_addr;
    rand size_t           q_size;
    rand len_t            q_len;
    rand reqrsp_pkg::amo_op_e         q_amo;
    rand ssr_user_t         q_ssr_user;

    constraint no_amo {
       q_amo == reqrsp_pkg::AMONone;
    }

    constraint size_limit {
       q_size < 2'b11;
       (q_ssr_user.indirect_enable == 1) -> q_ssr_user.user.indirect.index_size < 2'b11;
    }

    constraint align_transfer {
      (q_size == 2'b01) -> q_addr[0] == '0;
      (q_size == 2'b10) -> q_addr[1:0] == '0;
      (q_ssr_user.indirect_enable == 1 && q_ssr_user.user.indirect.index_size == 2'b01) -> (q_addr[0]+q_ssr_user.user.indirect.index_base_offset[0]) == '0;
      (q_ssr_user.indirect_enable == 1 && q_ssr_user.user.indirect.index_size == 2'b10) -> (q_addr[1:0]+q_ssr_user.user.indirect.index_base_offset[1:0]) == '0;
    }

  
  endclass


  class lsu_store_t #(
    parameter int DW = 32
  );
  typedef  logic[DW-1:0] data_t; 
  rand data_t            s_data;
  rand logic             s_last;
  endclass

  class lsu_load_t #(
    parameter int DW = 32
  );
  typedef  logic[DW-1:0] data_t; 
  rand data_t           l_data;
  rand logic            l_last;
  rand logic            l_error;
  endclass

  /// A master for the lsu REQRSP interface.
  class lsu_stream_master #(
    parameter int  AW = -1,
    parameter int  DW = -1,
    parameter time TA = 0 , // stimuli application time
    parameter time TT = 0   // stimuli test time
  );

    localparam int default_cycle_number =10;

    typedef lsu_req_t #(
        .AW(AW)
      ) para_lsu_req_t;

    typedef lsu_store_t #(
        .DW(DW)
      ) para_lsu_store_t;

    typedef lsu_load_t #(
        .DW(DW)
      ) para_lsu_load_t;

    virtual LSU_STREAM_BUS_DV #(
      .AW(AW),
      .DW(DW)
    ) bus;

    function new(
      virtual LSU_STREAM_BUS_DV #(
        .AW(AW),
        .DW(DW)
      ) bus
    );
      this.bus = bus;
    endfunction

    task cycle_start;
      #TT;
    endtask

    task cycle_end;
      @(posedge bus.clk);
    endtask

    task lsu_reset();
      bus.q_write     <= '0;
      bus.q_signed    <= '0;
      bus.q_addr      <= '0;
      bus.q_len       <= '0;
      bus.q_size      <= '0;
      bus.q_amo       <= reqrsp_pkg::AMONone;
      bus.q_ssr_user  <= '0;
      bus.q_valid     <= '0;
      bus.s_data      <= '0;
      bus.s_last      <= '0;
      bus.s_valid     <= '0;
      bus.l_ready     <= '0;
      
    endtask

    task send_lsu_req (input para_lsu_req_t req);
      bus.q_write     <= #TA req.q_write;
      bus.q_signed    <= #TA req.q_signed;
      bus.q_addr      <= #TA req.q_addr;
      bus.q_size      <= #TA req.q_size;
      bus.q_len       <= #TA req.q_len;
      bus.q_amo       <= #TA req.q_amo;
      bus.q_ssr_user  <= #TA req.q_ssr_user;

      bus.q_valid     <= #TA 1;
      cycle_start();
      while (bus.q_ready != 1) begin cycle_end(); cycle_start(); end
      cycle_end();

      bus.q_write     <= #TA '0;
      bus.q_signed    <= #TA '0;
      bus.q_addr      <= #TA '0;
      bus.q_len       <= #TA '0;
      bus.q_size      <= #TA '0;
      bus.q_amo       <= #TA reqrsp_pkg::AMONone;
      bus.q_ssr_user  <= #TA '0;

      bus.q_valid     <= #TA '0;

    endtask

    task send_store_data(input para_lsu_store_t store);
      bus.s_data      <= #TA  store.s_data;
      bus.s_last      <= #TA  store.s_last;
      bus.s_valid     <= #TA  1;

      cycle_start();
      while (bus.s_ready != 1) begin cycle_end(); cycle_start(); end
      cycle_end();

      bus.s_data      <= #TA '0;
      bus.s_last      <= #TA '0;
      bus.s_valid     <= #TA '0;
      
    endtask 

    /// Receive a response.
    task recv_load_data (output para_lsu_load_t load);
      bus.l_ready <= #TA 1;
      cycle_start();
      while (bus.l_valid != 1) begin cycle_end(); cycle_start(); end
      load = new;
      load.l_data      = bus.l_data;
      load.l_last      = bus.l_last;
      load.l_error     = bus.l_error;
      cycle_end();
      bus.l_ready <= #TA 0;
    endtask


    
  endclass

  class lsu_stream_monitor #(
    parameter int  AW = -1,
    parameter int  DW = -1,
    parameter time TA = 0 , // stimuli application time
    parameter time TT = 0   // stimuli test time
  );
    typedef lsu_req_t #(
        .AW(AW)
      ) para_lsu_req_t;

    typedef lsu_store_t #(
        .DW(DW)
      ) para_lsu_store_t;

    typedef lsu_load_t #(
        .DW(DW)
      ) para_lsu_load_t;

    mailbox req_mbx = new, store_mbx = new, load_mbx = new;

    virtual LSU_STREAM_BUS_DV #(
      .AW(AW),
      .DW(DW)
    ) bus;

    function new(
      virtual LSU_STREAM_BUS_DV #(
        .AW(AW),
        .DW(DW)
      ) bus
    );
      this.bus = bus;
    endfunction

    task cycle_start;
      #TT;
    endtask

    task cycle_end;
      @(posedge bus.clk);
    endtask

    /// Monitor request.
    task mon_req (output para_lsu_req_t req);
      cycle_start();
      while (!(bus.q_valid && bus.q_ready)) begin cycle_end(); cycle_start(); end
      req = new;
      req.q_write     = bus.q_write;
      req.q_signed    = bus.q_signed;
      req.q_addr      = bus.q_addr;
      req.q_size      = bus.q_size;
      req.q_len       = bus.q_len;
      req.q_amo       = bus.q_amo;
      req.q_ssr_user  = bus.q_ssr_user;
      cycle_end();
    endtask

    /// Monitor store .
    task mon_store (output para_lsu_store_t store);
      cycle_start();
      while (!(bus.s_valid && bus.s_ready)) begin cycle_end(); cycle_start(); end
      store = new;
      store.s_data     = bus.s_data;
      store.s_last     = bus.s_last;
      cycle_end();
    endtask

    /// Monitor load.
    task mon_load (output para_lsu_load_t load);
      cycle_start();
      while (!(bus.l_valid && bus.l_ready)) begin cycle_end(); cycle_start(); end
      load = new;
      load.l_data      = bus.l_data;
      load.l_last      = bus.l_last;
      load.l_error     = bus.l_error;
      cycle_end();
    endtask

    // Reqrsp Monitor.
    task monitor;
      fork
        forever begin
          automatic para_lsu_req_t req;
          mon_req(req);
          req_mbx.put(req);
        end
        forever begin
          automatic para_lsu_store_t store;
          mon_store(store);
          store_mbx.put(store);
        end
        forever begin
          automatic para_lsu_load_t load;
          mon_load(load);
          load_mbx.put(load);
        end
      join
    endtask


  endclass 


endpackage