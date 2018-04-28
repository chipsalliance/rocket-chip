Verilog simulation w/ Icarus Verilog
================
Jimmy Situ (web@jimmystone.cn)

Simulation with Icarus Verilog(IVL) is almost the same with VCS or Veriloator.

### Building The Project

First, to build the IVL simulator:
```bash
cd ivlsim
make
```
Then, you can run a set of assembly tests or simple benchmarks (Assuming you have N cores on your host system):
```bash
make -jN run-asm-tests
make -jN run-bmark-tests
```

To build a IVL simulator that is capable of VCD waveform generation:
```bash
cd ivlsim
make debug
```

And to run the assembly tests on the C simulator and generate waveforms:
```bash
make -jN run-asm-tests-debug
make -jN run-bmark-tests-debug
```