#pragma once

#include "auto_sig.hpp"
#include <cstdint>
#include <cstring>
#include <set>

/*
    We have defined 3 types of AXI signals for a different purposes: axi4,
   axi4_ptr, axi4_ref.

    Since verilator exposes signals as a value itself, we use axi4_ptr to get
   signal to connect.

    Then axi4_ptr can be converted to axi4_ref to reach the value in a better
   way.
 */

template <unsigned int A_WIDTH, unsigned int D_WIDTH, unsigned int ID_WIDTH>
struct axi4;

template <unsigned int A_WIDTH = 64, unsigned int D_WIDTH = 64,
          unsigned int ID_WIDTH = 4>
struct axi4_ptr {
  static_assert(__builtin_popcount(D_WIDTH) == 1,
                "D_WIDTH should be the power of 2.");
  static_assert(D_WIDTH >= 8, "D_WIDTH should be larger or equal to 8.");
  // aw
  AUTO_IN(*awid, ID_WIDTH - 1, 0) = NULL;
  AUTO_IN(*awaddr, A_WIDTH - 1, 0) = NULL;
  AUTO_IN(*awlen, 7, 0) = NULL;
  AUTO_IN(*awsize, 2, 0) = NULL;
  AUTO_IN(*awburst, 1, 0) = NULL;
  AUTO_IN(*awvalid, 0, 0) = NULL;
  AUTO_OUT(*awready, 0, 0) = NULL;
  // w
  AUTO_IN(*wdata, D_WIDTH - 1, 0) = NULL;
  AUTO_IN(*wstrb, (D_WIDTH / 8) - 1, 0) = NULL;
  AUTO_IN(*wlast, 0, 0) = NULL;
  AUTO_IN(*wvalid, 0, 0) = NULL;
  AUTO_OUT(*wready, 0, 0) = NULL;
  // b
  AUTO_OUT(*bid, ID_WIDTH - 1, 0) = NULL;
  AUTO_OUT(*bresp, 1, 0) = NULL;
  AUTO_OUT(*bvalid, 0, 0) = NULL;
  AUTO_IN(*bready, 0, 0) = NULL;
  // ar
  AUTO_IN(*arid, ID_WIDTH - 1, 0) = NULL;
  AUTO_IN(*araddr, A_WIDTH - 1, 0) = NULL;
  AUTO_IN(*arlen, 7, 0) = NULL;
  AUTO_IN(*arsize, 2, 0) = NULL;
  AUTO_IN(*arburst, 1, 0) = NULL;
  AUTO_IN(*arvalid, 0, 0) = NULL;
  AUTO_OUT(*arready, 0, 0) = NULL;
  // r
  AUTO_OUT(*rid, ID_WIDTH - 1, 0) = NULL;
  AUTO_OUT(*rdata, D_WIDTH - 1, 0) = NULL;
  AUTO_OUT(*rresp, 1, 0) = NULL;
  AUTO_OUT(*rlast, 0, 0) = NULL;
  AUTO_OUT(*rvalid, 0, 0) = NULL;
  AUTO_IN(*rready, 0, 0) = NULL;
  bool check() {
    std::set<void *> s;
    // aw
    s.insert((void *)awid);
    s.insert((void *)awaddr);
    s.insert((void *)awlen);
    s.insert((void *)awsize);
    s.insert((void *)awburst);
    s.insert((void *)awvalid);
    s.insert((void *)awready);
    // w
    s.insert((void *)wdata);
    s.insert((void *)wstrb);
    s.insert((void *)wlast);
    s.insert((void *)wvalid);
    s.insert((void *)wready);
    // b
    s.insert((void *)bid);
    s.insert((void *)bresp);
    s.insert((void *)bvalid);
    s.insert((void *)bready);
    // ar
    s.insert((void *)arid);
    s.insert((void *)araddr);
    s.insert((void *)arlen);
    s.insert((void *)arsize);
    s.insert((void *)arburst);
    s.insert((void *)arvalid);
    s.insert((void *)arready);
    // r
    s.insert((void *)rid);
    s.insert((void *)rdata);
    s.insert((void *)rresp);
    s.insert((void *)rlast);
    s.insert((void *)rvalid);
    s.insert((void *)rready);
    return s.size() == 29 && s.count(NULL) == 0;
  }
};

template <unsigned int A_WIDTH = 64, unsigned int D_WIDTH = 64,
          unsigned int ID_WIDTH = 4>
struct axi4_ref {
  AUTO_IN(&awid, ID_WIDTH - 1, 0);
  AUTO_IN(&awaddr, A_WIDTH - 1, 0);
  AUTO_IN(&awlen, 7, 0);
  AUTO_IN(&awsize, 2, 0);
  AUTO_IN(&awburst, 1, 0);
  AUTO_IN(&awvalid, 0, 0);
  AUTO_OUT(&awready, 0, 0);
  // w
  AUTO_IN(&wdata, D_WIDTH - 1, 0);
  AUTO_IN(&wstrb, (D_WIDTH / 8) - 1, 0);
  AUTO_IN(&wlast, 0, 0);
  AUTO_IN(&wvalid, 0, 0);
  AUTO_OUT(&wready, 0, 0);
  // b
  AUTO_OUT(&bid, ID_WIDTH - 1, 0);
  AUTO_OUT(&bresp, 1, 0);
  AUTO_OUT(&bvalid, 0, 0);
  AUTO_IN(&bready, 0, 0);
  // ar
  AUTO_IN(&arid, ID_WIDTH - 1, 0);
  AUTO_IN(&araddr, A_WIDTH - 1, 0);
  AUTO_IN(&arlen, 7, 0);
  AUTO_IN(&arsize, 2, 0);
  AUTO_IN(&arburst, 1, 0);
  AUTO_IN(&arvalid, 0, 0);
  AUTO_OUT(&arready, 0, 0);
  // r
  AUTO_OUT(&rid, ID_WIDTH - 1, 0);
  AUTO_OUT(&rdata, D_WIDTH - 1, 0);
  AUTO_OUT(&rresp, 1, 0);
  AUTO_OUT(&rlast, 0, 0);
  AUTO_OUT(&rvalid, 0, 0);
  AUTO_IN(&rready, 0, 0);
  axi4_ref(axi4_ptr<A_WIDTH, D_WIDTH, ID_WIDTH> &ptr)
      : awid(*(ptr.awid)), awaddr(*(ptr.awaddr)), awlen(*(ptr.awlen)),
        awsize(*(ptr.awsize)), awburst(*(ptr.awburst)), awvalid(*(ptr.awvalid)),
        awready(*(ptr.awready)), wdata(*(ptr.wdata)), wstrb(*(ptr.wstrb)),
        wlast(*(ptr.wlast)), wvalid(*(ptr.wvalid)), wready(*(ptr.wready)),
        bid(*(ptr.bid)), bresp(*(ptr.bresp)), bvalid(*(ptr.bvalid)),
        bready(*(ptr.bready)), arid(*(ptr.arid)), araddr(*(ptr.araddr)),
        arlen(*(ptr.arlen)), arsize(*(ptr.arsize)), arburst(*(ptr.arburst)),
        arvalid(*(ptr.arvalid)), arready(*(ptr.arready)), rid(*(ptr.rid)),
        rdata(*(ptr.rdata)), rresp(*(ptr.rresp)), rlast(*(ptr.rlast)),
        rvalid(*(ptr.rvalid)), rready(*(ptr.rready)) {}

  axi4_ref(axi4<A_WIDTH, D_WIDTH, ID_WIDTH> &axi4);
};

template <unsigned int A_WIDTH = 64, unsigned int D_WIDTH = 64,
          unsigned int ID_WIDTH = 4>
struct axi4 {
  AUTO_IN(awid, ID_WIDTH - 1, 0);
  AUTO_IN(awaddr, A_WIDTH - 1, 0);
  AUTO_IN(awlen, 7, 0);
  AUTO_IN(awsize, 2, 0);
  AUTO_IN(awburst, 1, 0);
  AUTO_IN(awvalid, 0, 0);
  AUTO_OUT(awready, 0, 0);
  // w
  AUTO_IN(wdata, D_WIDTH - 1, 0);
  AUTO_IN(wstrb, (D_WIDTH / 8) - 1, 0);
  AUTO_IN(wlast, 0, 0);
  AUTO_IN(wvalid, 0, 0);
  AUTO_OUT(wready, 0, 0);
  // b
  AUTO_OUT(bid, ID_WIDTH - 1, 0);
  AUTO_OUT(bresp, 1, 0);
  AUTO_OUT(bvalid, 0, 0);
  AUTO_IN(bready, 0, 0);
  // ar
  AUTO_IN(arid, ID_WIDTH - 1, 0);
  AUTO_IN(araddr, A_WIDTH - 1, 0);
  AUTO_IN(arlen, 7, 0);
  AUTO_IN(arsize, 2, 0);
  AUTO_IN(arburst, 1, 0);
  AUTO_IN(arvalid, 0, 0);
  AUTO_OUT(arready, 0, 0);
  // r
  AUTO_OUT(rid, ID_WIDTH - 1, 0);
  AUTO_OUT(rdata, D_WIDTH - 1, 0);
  AUTO_OUT(rresp, 1, 0);
  AUTO_OUT(rlast, 0, 0);
  AUTO_OUT(rvalid, 0, 0);
  AUTO_IN(rready, 0, 0);
  axi4() {
    // reset all pointer to zero
    memset(this, 0, sizeof(*this));
  }
  void update_input(axi4_ref<A_WIDTH, D_WIDTH, ID_WIDTH> &ref) {
    // aw
    awid = ref.awid;
    awaddr = ref.awaddr;
    awlen = ref.awlen;
    awsize = ref.awsize;
    awburst = ref.awburst;
    awvalid = ref.awvalid;
    // w
    wdata = ref.wdata;
    wstrb = ref.wstrb;
    wlast = ref.wlast;
    wvalid = ref.wvalid;
    // b
    bready = ref.bready;
    // arid
    arid = ref.arid;
    araddr = ref.araddr;
    arlen = ref.arlen;
    arsize = ref.arsize;
    arburst = ref.arburst;
    arvalid = ref.arvalid;
    // r
    rready = ref.rready;
  }
  void update_output(axi4_ref<A_WIDTH, D_WIDTH, ID_WIDTH> &ref) {
    ref.awready = awready;
    ref.wready = wready;
    ref.bid = bid;
    ref.bresp = bresp;
    ref.bvalid = bvalid;
    ref.arready = arready;
    ref.rid = rid;
    ref.rdata = rdata;
    ref.rresp = rresp;
    ref.rlast = rlast;
    ref.rvalid = rvalid;
  }
};

template <unsigned int A_WIDTH, unsigned int D_WIDTH, unsigned int ID_WIDTH>
axi4_ref<A_WIDTH, D_WIDTH, ID_WIDTH>::axi4_ref(
    axi4<A_WIDTH, D_WIDTH, ID_WIDTH> &axi4)
    : awid(axi4.awid), awaddr(axi4.awaddr), awlen(axi4.awlen),
      awsize(axi4.awsize), awburst(axi4.awburst), awvalid(axi4.awvalid),
      awready(axi4.awready), wdata(axi4.wdata), wstrb(axi4.wstrb),
      wlast(axi4.wlast), wvalid(axi4.wvalid), wready(axi4.wready),
      bid(axi4.bid), bresp(axi4.bresp), bvalid(axi4.bvalid),
      bready(axi4.bready), arid(axi4.arid), araddr(axi4.araddr),
      arlen(axi4.arlen), arsize(axi4.arsize), arburst(axi4.arburst),
      arvalid(axi4.arvalid), arready(axi4.arready), rid(axi4.rid),
      rdata(axi4.rdata), rresp(axi4.rresp), rlast(axi4.rlast),
      rvalid(axi4.rvalid), rready(axi4.rready) {}

enum axi_resp {
  RESP_OKEY = 0,
  RESP_EXOKEY = 1,
  RESP_SLVERR = 2,
  RESP_DECERR = 3
};

enum axi_burst_type {
  BURST_FIXED = 0,
  BURST_INCR = 1,
  BURST_WRAP = 2,
  BURST_RESERVED = 3
};
