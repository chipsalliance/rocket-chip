base_dir = $(abspath ..)

CXXSRCS := comlog float_fix
CXXFLAGS := $(CXXFLAGS) -std=c++11 -Wall

OBJS := $(addsuffix .o,$(CXXSRCS))
PROGRAMS := $(CXXSRCS)

all: $(PROGRAMS)

%: %.o
	$(CXX) $< $(LDFLAGS) -o $@

%.o: $(base_dir)/csrc/%.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(PROGRAMS)
