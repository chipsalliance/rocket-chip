// See LICENSE.SiFive for license details.

#include <fesvr/dtm.h>
#include <vpi_user.h>
#include <sv_vpi_user.h>
//#include <svdpi.h>
#include <assert.h>

dtm_t* dtm;

static PLI_INT32 debug_tick_calltf(PLI_BYTE8 *nouse)
{

  unsigned char  debug_req_valid;     //0
  unsigned char  debug_req_ready;     //1
  int            debug_req_bits_addr; //2
  int            debug_req_bits_op;   //3
  int            debug_req_bits_data; //4
  unsigned char  debug_resp_valid;    //5
  unsigned char  debug_resp_ready;    //6
  int            debug_resp_bits_resp;//7
  int            debug_resp_bits_data;//8
  
  //vpi_printf("debug_tick_calltf started\n");
  
  vpiHandle sys = vpi_handle(vpiSysTfCall, 0);
  vpiHandle argv = vpi_iterate(vpiArgument, sys);
  vpiHandle arg[9];
  s_vpi_value val[9];
  s_vpi_value rtn;
  
  // link all signals
  for(int i = 0; i < 9; i++) {
    arg[i] = vpi_scan(argv);
    assert(arg[i] != 0);

    val[i].format = vpiIntVal;
  }
  vpi_free_object(argv);

  if (!dtm) {
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info))
      abort();
      dtm = new dtm_t(info.argc, info.argv);
  }

  dtm_t::resp resp_bits;
 
  // Get value
  vpi_get_value(arg[1], &(val[1]));
  vpi_get_value(arg[5], &(val[5]));
  vpi_get_value(arg[7], &(val[7]));
  vpi_get_value(arg[8], &(val[8]));
  debug_req_ready      = (unsigned char)(val[1].value.integer);
  debug_resp_valid     = (unsigned char)(val[5].value.integer);
  debug_resp_bits_resp = val[7].value.integer;
  debug_resp_bits_data = val[8].value.integer;

  resp_bits.resp = debug_resp_bits_resp;
  resp_bits.data = debug_resp_bits_data;

  dtm->tick
  (
    debug_req_ready,
    debug_resp_valid,
    resp_bits
  );

  debug_resp_ready = dtm->resp_ready();
  debug_req_valid = dtm->req_valid();
  debug_req_bits_addr = dtm->req_bits().addr;
  debug_req_bits_op = dtm->req_bits().op;
  debug_req_bits_data = dtm->req_bits().data;
  
  // Put value
  val[6].value.integer = debug_resp_ready;
  val[0].value.integer = debug_req_valid;
  val[2].value.integer = debug_req_bits_addr;
  val[3].value.integer = debug_req_bits_op;
  val[4].value.integer = debug_req_bits_data;

  vpi_put_value(arg[6], &(val[6]), 0, vpiNoDelay); 
  vpi_put_value(arg[0], &(val[0]), 0, vpiNoDelay); 
  vpi_put_value(arg[2], &(val[2]), 0, vpiNoDelay); 
  vpi_put_value(arg[3], &(val[3]), 0, vpiNoDelay); 
  vpi_put_value(arg[4], &(val[4]), 0, vpiNoDelay); 

  //return value
  rtn.format = vpiIntVal;
  rtn.value.integer = dtm->done() ? (dtm->exit_code() << 1 | 1) : 0; 
  vpi_put_value(sys, &rtn, 0, vpiNoDelay);
  return 0;
}

PLI_INT32 debug_tick_sizetf(PLI_BYTE8 *nouse)
{
  return 32;
}

void debug_tick_register(void)
{
      s_vpi_systf_data tf_data;

      tf_data.type        = vpiSysFunc;
      tf_data.sysfunctype = vpiIntFunc;
      tf_data.tfname    = "$debug_tick";
      tf_data.calltf    = debug_tick_calltf;
      tf_data.sizetf    = 0; //debug_tick_sizetf;
      tf_data.compiletf = 0;
      vpi_register_systf(&tf_data);
}

