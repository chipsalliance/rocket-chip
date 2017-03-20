// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

#include "verilated.h"
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif
#include <fesvr/dtm.h>
#include <iostream>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

extern dtm_t* dtm;
static uint64_t trace_count = 0;
bool verbose;
bool done_reset;

void handle_sigterm(int sig)
{
  dtm->stop();
}

double sc_time_stamp()
{
  return trace_count;
}

extern "C" int vpi_get_vlog_info(void* arg)
{
  return 0;
}

static void usage(const char * program_name)
{
  printf("Usage: %s [EMULATOR OPTION]... [HOST OPTION]... BINARY [TARGET OPTION]...\n",
         program_name);
  fputs("\
Run a BINARY on the Rocket Chip emulator.\n\
\n\
Mandatory arguments to long options are mandatory for short options too.\n\
\n\
EMULATOR OPTIONS\n\
  -c, --cycle-count          Print the cycle count before exiting\n\
       +cycle-count\n\
  -h, --help                 Display this help and exit\n\
  -m, --max-cycles=[CYCLES]  Kill the emulation after CYCLES\n\
       +max-cycles=[CYCLES]\n\
  -s, --seed=[SEED]          Use random number seed SEED\n\
  -V, --verbose              Enable all Chisel printfs\n\
       +verbose\n\
", stdout);
#if VM_TRACE
  fputs("\
  -v, --vcd=[FILE],          write vcd trace to FILE (or '-' for stdout)\n\
  -x, --dump-start=[CYCLE]   start VCD tracing at CYCLE\n\
      +dump-start\n\
\n\
EMULATOR OPTIONS (unsupported in non-debug build -- try `make debug`)\n\
", stdout);
#endif
  fputs("\
VCD options (e.g., -v, +dump-start) require a debug-enabled emulator.\n\
Try `make debug`.\n\
", stdout);
  fputs(HTIF_USAGE_OPTIONS, stdout);
}

int main(int argc, char** argv)
{
  unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  uint64_t max_cycles = -1;
  int ret = 0;
  bool print_cycles = false;
#if VM_TRACE
  FILE * vcdfile = NULL;
  uint64_t start = 0;
#endif
  char ** htif_argv = NULL;

  while (1) {
    static struct option long_options[] = {
      {"cycle-count", no_argument,       0, 'c' },
      {"help",        no_argument,       0, 'h' },
      {"max-cycles",  required_argument, 0, 'm' },
      {"seed",        required_argument, 0, 's' },
      {"verbose",     no_argument,       0, 'V' },
#if VM_TRACE
      {"vcd",         required_argument, 0, 'v' },
      {"dump-start",  required_argument, 0, 'x' },
#endif
      HTIF_LONG_OPTIONS
    };
    int option_index = 0;
#if VM_TRACE
    int c = getopt_long(argc, argv, "-chm:s:v:Vx:", long_options, &option_index);
#else
    int c = getopt_long(argc, argv, "-chm:s:V", long_options, &option_index);
#endif
    if (c == -1) break;
 retry:
    switch (c) {
      // Process long and short EMULATOR options
      case '?': usage(argv[0]);             return 1;
      case 'c': print_cycles = true;        break;
      case 'h': usage(argv[0]);             return 0;
      case 'm': max_cycles = atoll(optarg); break;
      case 's': random_seed = atoi(optarg); break;
      case 'V': verbose = true;             break;
#if VM_TRACE
      case 'v': {
        vcdfile = strcmp(optarg, "-") == 0 ? stdout : fopen(optarg, "w");
        if (!vcdfile) {
          std::cerr << "Unable to open " << optarg << " for VCD write\n";
          return 1;
        }
        break;
      }
      case 'x': start = atoll(optarg);      break;
#endif
      // Process legacy '+' EMULATOR arguments by replacing them with
      // their getopt equivalents
      case 1: {
        std::string arg = optarg;
        if (arg.substr(0, 1) != "+") {
          optind--;
          goto done_processing;
        }
        if (arg == "+verbose")
          c = 'V';
        else if (arg.substr(0, 12) == "+max-cycles=") {
          c = 'm';
          optarg = optarg+12;
        }
#if VM_TRACE
        else if (arg.substr(0, 12) == "+dump-start=")
          c = 'x';
          optarg = optarg+12;
        }
#endif
        else if (arg.substr(0, 12) == "+cycle-count")
          c = 'c';
        // If we don't find a legacy '+' argument, it still could be
        // an HTIF (HOST) argument and not an error. If this is the
        // case, then we're doing processing EMULATOR arguments.
        else {
          static struct option htif_long_options [] = { HTIF_LONG_OPTIONS };
          struct option * htif_option = &htif_long_options[0];
          while (htif_option->name) {
            if (arg.substr(1, strlen(htif_option->name)) == htif_option->name) {
              optind--;
              goto done_processing;
            }
            htif_option++;
          }
          std::cerr << argv[0] << ": invalid HTIF legacy plus-arg \"" << arg << "\"\n";
          c = '?';
        }
        goto retry;
      }
      // Realize that we've hit HTIF (HOST) arguments or error out
      default:
        if (c >= HTIF_LONG_OPTIONS_OPTIND) {
          optind--;
          goto done_processing;
        }
        c = '?';
        goto retry;
    }
  }

done_processing:
  if (optind == argc) {
    std::cerr << "No binary specified for emulator\n";
    usage(argv[0]);
    return 1;
  }
  int htif_argc = 1 + argc - optind;
  htif_argv = (char **) malloc((htif_argc) * sizeof (char *));
  htif_argv[0] = argv[0];
  for (int i = 1; optind < argc;) htif_argv[i++] = argv[optind++];

  if (verbose)
    fprintf(stderr, "using random seed %u\n", random_seed);

  srand(random_seed);
  srand48(random_seed);

  Verilated::randReset(2);
  Verilated::commandArgs(argc, argv);
  TEST_HARNESS *tile = new TEST_HARNESS;

#if VM_TRACE
  Verilated::traceEverOn(true); // Verilator must compute traced signals
  std::unique_ptr<VerilatedVcdFILE> vcdfd(new VerilatedVcdFILE(vcdfile));
  std::unique_ptr<VerilatedVcdC> tfp(new VerilatedVcdC(vcdfd.get()));
  if (vcdfile) {
    tile->trace(tfp.get(), 99);  // Trace 99 levels of hierarchy
    tfp->open("");
  }
#endif

  dtm = new dtm_t(htif_argc, htif_argv);

  signal(SIGTERM, handle_sigterm);

  // reset for several cycles to handle pipelined reset
  for (int i = 0; i < 10; i++) {
    tile->reset = 1;
    tile->clock = 0;
    tile->eval();
    tile->clock = 1;
    tile->eval();
    tile->reset = 0;
  }
  done_reset = true;

  while (!dtm->done() && !tile->io_success && trace_count < max_cycles) {
    tile->clock = 0;
    tile->eval();
#if VM_TRACE
    bool dump = tfp && trace_count >= start;
    if (dump)
      tfp->dump(static_cast<vluint64_t>(trace_count * 2));
#endif

    tile->clock = 1;
    tile->eval();
#if VM_TRACE
    if (dump)
      tfp->dump(static_cast<vluint64_t>(trace_count * 2 + 1));
#endif
    trace_count++;
  }

#if VM_TRACE
  if (tfp)
    tfp->close();
  if (vcdfile)
    fclose(vcdfile);
#endif

  if (dtm->exit_code())
  {
    fprintf(stderr, "*** FAILED *** (code = %d, seed %d) after %ld cycles\n", dtm->exit_code(), random_seed, trace_count);
    ret = dtm->exit_code();
  }
  else if (trace_count == max_cycles)
  {
    fprintf(stderr, "*** FAILED *** (timeout, seed %d) after %ld cycles\n", random_seed, trace_count);
    ret = 2;
  }
  else if (verbose || print_cycles)
  {
    fprintf(stderr, "Completed after %ld cycles\n", trace_count);
  }

  if (dtm) delete dtm;
  if (tile) delete tile;
  if (htif_argv) free(htif_argv);
  return ret;
}
