// Author: Chi Zhang <chizhang@ethz.ch>
// Macros to define LSU Channel and Request/Response Structs
`ifndef LSU_TYPEDEF_SVH_
`define LSU_TYPEDEF_SVH_

`define LSU_TYPEDEF_Q_CHAN_T(q_chan_t, addr_t)  	\
  typedef struct packed {                                       \
    logic               q_write;                                \
    logic               q_signed;                               \
    addr_t              q_addr;                                 \
    axi_pack_pkg::size_t   			      q_size;                                 \
    axi_pack_pkg::len_t    		   	    q_len;                                  \
    reqrsp_pkg::amo_op_e			      q_amo;									                \
    axi_pack_pkg::ssr_user_t       q_ssr_user;                   \
  } q_chan_t;

`define LSU_TYPEDEF_S_CHAN_T(s_chan_t, data_t)  				        \
  typedef struct packed {                                       \
  	data_t  			      s_data;									                \
    logic               s_last;                               	\
  } s_chan_t;

`define LSU_TYPEDEF_L_CHAN_T(l_chan_t, data_t)  				\
  typedef struct packed {                                       \
  	data_t  			      l_data;									                \
    logic               l_last;                               	\
    logic               l_error;                               	\
  } l_chan_t;

`define LSU_TYPEDEF_REQ_T(req_t, q_chan_t, s_chan_t)  \
  typedef struct packed {                                         \
    q_chan_t  q;                                                  \
    logic     q_valid;                                            \
    s_chan_t  s;                                                  \
    logic     s_valid;                                            \
    logic     l_ready;                                            \
  } req_t;

`define LSU_TYPEDEF_RESP_T(resp_t, l_chan_t)  			\
  typedef struct packed {                               \
    logic     q_ready;                                 	\
    logic     s_ready;                                 	\
    l_chan_t  l;                                        \
    logic     l_valid;                                  \
  } resp_t;

`define LSU_TYPEDEF_ALL(__name, addr_t, data_t)        \
  `LSU_TYPEDEF_Q_CHAN_T(__name``_q_chan_t, addr_t)     \
  `LSU_TYPEDEF_S_CHAN_T(__name``_s_chan_t, data_t)                        		\
  `LSU_TYPEDEF_L_CHAN_T(__name``_l_chan_t, data_t)                    			\
  `LSU_TYPEDEF_REQ_T(__name``_req_t, __name``_q_chan_t, __name``_s_chan_t) 		\
  `LSU_TYPEDEF_RESP_T(__name``_rsp_t, __name``_l_chan_t)

`endif