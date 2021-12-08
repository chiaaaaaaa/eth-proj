interface LSU_STREAM_BUS_DV #(
  parameter int AW = -1,
  parameter int DW = -1
)(
	input logic clk
);

import axi_pack_pkg::*;


//TYPE DIFINITION
typedef  logic[AW-1:0] addr_t; 
typedef  logic[DW-1:0] data_t; 

//stream request channel 
 logic            q_write;
 logic            q_signed;
 addr_t 		   		q_addr;
 size_t			  		q_size;
 len_t            q_len;
 reqrsp_pkg::amo_op_e  				q_amo;
 ssr_user_t       q_ssr_user; 
 logic 					  q_valid;
 logic 					  q_ready;

//stream store channel
 data_t           s_data;
 logic            s_last;
 logic            s_valid;
 logic            s_ready;

//response signals
 data_t 				  l_data;
 logic            l_last;
 logic 					  l_error;
 logic 					  l_valid;
 logic 					  l_ready;

endinterface 