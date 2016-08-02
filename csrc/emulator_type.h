// See LICENSE for license details.

#ifndef VERILATOR
# define bool_t dat_t<1>
# define values(x) ((x)->values)
# define field(name) &(tile.Top__ ## name)
#else
# define bool_t CData
# define values(x) (x)
# define field(name) &(tile.name)
#endif
#define value(x) (*values(x))

#ifndef VERILATOR
#define mem_addr_t dat_t<MEM_ADDR_BITS>
#elif MEM_ADDR_BITS <= 8
#define mem_addr_t CData
#elif MEM_ADDR_BITS <= 16
#define mem_addr_t SData
#elif MEM_ADDR_BITS <= 32
#define mem_addr_t IData
#elif MEM_ADDR_BITS <= 64
#define mem_addr_t QData
#else // MEM_ADDR_BITS > 64
#define mem_addr_t WData*
#endif

#ifndef VERILATOR
#define mem_id_t dat_t<MEM_ID_BITS>
#elif MEM_ID_BITS <= 8
#define mem_id_t CData
#elif MEM_ID_BITS <= 16
#define mem_id_t SData
#elif MEM_ID_BITS <= 32
#define mem_id_t IData
#elif MEM_ID_BITS <= 64
#define mem_id_t QData
#else // MEM_ID_BITS > 64
#define mem_id_t WData*
#endif

#ifndef VERILATOR
#define mem_size_t dat_t<MEM_SIZE_BITS>
#elif MEM_SIZE_BITS <= 8
#define mem_size_t CData
#elif MEM_SIZE_BITS <= 16
#define mem_size_t SData
#elif MEM_SIZE_BITS <= 32
#define mem_size_t IData
#elif MEM_SIZE_BITS <= 64
#define mem_size_t QData
#else // MEM_SIZE_BITS > 64
#define mem_size_t WData*
#endif

#ifndef VERILATOR
#define mem_len_t dat_t<MEM_LEN_BITS>
#elif MEM_LEN_BITS <= 8
#define mem_len_t CData
#elif MEM_LEN_BITS <= 16
#define mem_len_t SData
#elif MEM_LEN_BITS <= 32
#define mem_len_t IData
#elif MEM_LEN_BITS <= 64
#define mem_len_t QData
#else // MEM_LEN_BITS > 64
#define mem_len_t WData*
#endif

#ifndef VERILATOR
#define mem_strb_t dat_t<MEM_STRB_BITS>
#elif MEM_STRB_BITS <= 8
#define mem_strb_t CData
#elif MEM_STRB_BITS <= 16
#define mem_strb_t SData
#elif MEM_STRB_BITS <= 32
#define mem_strb_t IData
#elif MEM_STRB_BITS <= 64
#define mem_strb_t QData
#else // MEM_STRB_BITS > 64
#define mem_strb_t WData*
#endif

#ifndef VERILATOR
#define mem_data_t dat_t<MEM_DATA_BITS>
#elif MEM_DATA_BITS <= 8
#define mem_data_t CData
#elif MEM_DATA_BITS <= 16
#define mem_data_t SData
#elif MEM_DATA_BITS <= 32
#define mem_data_t IData
#elif MEM_DATA_BITS <= 64
#define mem_data_t QData
#else // MEM_DATA_BITS > 64
#define mem_data_t WData*
#endif

#ifndef VERILATOR
#define mem_resp_t dat_t<MEM_RESP_BITS>
#elif MEM_RESP_BITS <= 8
#define mem_resp_t CData
#elif MEM_RESP_BITS <= 16
#define mem_resp_t SData
#elif MEM_RESP_BITS <= 32
#define mem_resp_t IData
#elif MEM_RESP_BITS <= 64
#define mem_resp_t QData
#else // MEM_RESP_BITS > 64
#define mem_resp_t WData*
#endif
