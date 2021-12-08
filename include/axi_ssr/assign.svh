// Author: Chi Zhang <chizhang@ethz.ch>
// Macros to assign LSU Interfaces and Structs

`ifndef LSU_ASSIGN_SVH_
`define LSU_ASSIGN_SVH_

`define LSU_ASSIGN_INTF_TO_REQRESP(intf,req,resp) 	\
  assign req.q.q_write = intf.q_write;				\
  assign req.q.q_signed = intf.q_signed;			\
  assign req.q.q_addr = intf.q_addr;				\
  assign req.q.q_size = intf.q_size;				\
  assign req.q.q_len = intf.q_len;					\
  assign req.q.q_amo = intf.q_amo;					\
  assign req.q.q_ssr_user = intf.q_ssr_user;    \
  assign req.q_valid = intf.q_valid;				\
  assign req.s.s_data = intf.s_data;				\
  assign req.s.s_last = intf.s_last;				\
  assign req.s_valid = intf.s_valid;				\
  assign req.l_ready = intf.l_ready;				\
  assign intf.q_ready = resp.q_ready;				\
  assign intf.s_ready = resp.s_ready;				\
  assign intf.l_data = resp.l.l_data;				\
  assign intf.l_last = resp.l.l_last;				\
  assign intf.l_error = resp.l.l_error;				\
  assign intf.l_valid = resp.l_valid;




`endif