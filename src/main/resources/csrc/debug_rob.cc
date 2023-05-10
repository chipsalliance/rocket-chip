#include <map>
#include <deque>
#include <stdint.h>
#include <cstring>

#define WDATA_BITS (512)
#define WDATA_BYTES (WDATA_BITS / 8)

typedef struct tagged_wb_data_t {
  uint64_t tag;
  uint8_t data[WDATA_BYTES];
};

typedef struct tagged_traced_insn_t {
  uint64_t iaddr;
  uint64_t insn;
  uint8_t priv;
  bool exception;
  bool interrupt;
  uint64_t cause;
  uint64_t tval;
  uint8_t wdata[WDATA_BYTES];

  bool waiting;
  uint64_t wb_tag;
};

typedef struct debug_rob_t {
  std::deque<tagged_traced_insn_t*> rob;
  std::deque<tagged_wb_data_t*> wb_datas;
};

std::map<int, debug_rob_t*> debug_robs;

extern "C" void debug_rob_push_trace(int hartid,
				     char should_wb,
				     char has_wb,
				     long long int wb_tag,
				     char trace_valid,
				     long long int trace_iaddr,
				     long long int trace_insn,
				     int trace_priv,
				     char trace_exception,
				     char trace_interrupt,
				     long long int trace_cause,
				     long long int trace_tval,
				     long long int* trace_wdata) {

  if (debug_robs.find(hartid) == debug_robs.end())
    debug_robs[hartid] = new debug_rob_t;

  if (!trace_valid) return;

  tagged_traced_insn_t* insn = new tagged_traced_insn_t;
  insn->iaddr = trace_iaddr;
  insn->insn = trace_insn;
  insn->priv = trace_priv;
  insn->exception = trace_exception;
  insn->interrupt = trace_interrupt;
  insn->cause = trace_cause;
  insn->tval = trace_tval;
  insn->waiting = should_wb && !has_wb;
  insn->wb_tag = wb_tag;
  memcpy(insn->wdata, trace_wdata, WDATA_BYTES);

  debug_robs[hartid]->rob.push_back(insn);
}

extern "C" void debug_rob_push_wb(int hartid,
				  char valid,
				  long long int wb_tag,
				  long long int* wb_data) {
  if (debug_robs.find(hartid) == debug_robs.end())
    debug_robs[hartid] = new debug_rob_t;

  if (!valid) return;

  tagged_wb_data_t* data = new tagged_wb_data_t;
  data->tag = wb_tag;
  memcpy(data->data, wb_data, WDATA_BYTES);
  debug_robs[hartid]->wb_datas.push_back(data);
}

extern "C" void debug_rob_pop_trace(int hartid,
				    char* trace_valid,
				    long long int* trace_iaddr,
				    long long int* trace_insn,
				    int* trace_priv,
				    char* trace_exception,
				    char* trace_interrupt,
				    long long int* trace_cause,
				    long long int* trace_tval,
				    long long int* trace_wdata) {
  *trace_valid = 0;
  if (debug_robs.find(hartid) == debug_robs.end()) return;
  if (debug_robs[hartid]->rob.empty()) return;

  tagged_traced_insn_t* front = debug_robs[hartid]->rob.front();
  std::deque<tagged_wb_data_t*> &wb_datas = debug_robs[hartid]->wb_datas;
  if (front->waiting) {
    for (auto it = wb_datas.begin(); it != wb_datas.end(); it++) {
      if ((*it)->tag == front->wb_tag) {
        memcpy(front->wdata, (*it)->data, WDATA_BYTES);
        front->waiting = false;
        delete (*it);
        wb_datas.erase(it);
        break;
      }
    }
  }

  if (front->waiting) return;

  *trace_valid = true;
  *trace_iaddr = front->iaddr;
  *trace_insn = front->insn;
  *trace_priv = front->priv;
  *trace_exception = front->exception;
  *trace_interrupt = front->interrupt;
  *trace_cause = front->cause;
  *trace_tval = front->tval;
  memcpy(trace_wdata, front->wdata, WDATA_BYTES);
  debug_robs[hartid]->rob.pop_front();
  delete front;
}


