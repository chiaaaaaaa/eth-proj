package axi_pack_pkg;
	//Parameters
	parameter IndexOffsetWidth = 17;

	/// Size field.
	typedef logic [1:0] size_t;
	/// Length field. Same semantic as AXI.
	typedef logic [7:0] len_t;

	typedef logic [7:0] stride_t;

	typedef logic [5:0] nest_len_t;

	typedef logic [5:0] nest_stride_t;

	typedef logic [2:0] index_size_t;

	typedef logic [IndexOffsetWidth-1:0] index_base_offset_t;

	typedef struct packed {
		stride_t		stride;
		nest_len_t	    nest_len;
		nest_stride_t	nest_stride;
	} affine_t;


	typedef struct packed {
		index_size_t		index_size;
		index_base_offset_t    index_base_offset;
	} indirect_t;

	typedef union packed {
		affine_t		affine;
		indirect_t 		indirect;
	} user_union_t;

	typedef struct packed {
		logic 			indirect_enable;
		user_union_t    user;	
	} ssr_user_t;
	

endpackage 