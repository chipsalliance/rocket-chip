package uncore

import Chisel._

object Constants
{

  val X_INIT_TYPE_MAX_BITS = 2
  val X_REP_TYPE_MAX_BITS = 3
  val P_REQ_TYPE_MAX_BITS = 2
  val P_REP_TYPE_MAX_BITS = 3

  val PADDR_BITS = 40;
  val VADDR_BITS = 43;
  val PGIDX_BITS = 13;
  val PPN_BITS = PADDR_BITS-PGIDX_BITS;
  val VPN_BITS = VADDR_BITS-PGIDX_BITS;
  val ASID_BITS = 7;
  val PERM_BITS = 6;


  val COHERENCE_DATA_BITS = (1 << OFFSET_BITS)*8 
  val TILE_ID_BITS = log2Up(NTILES)+1
  val TILE_XACT_ID_BITS = log2Up(NMSHR)+3
  val NGLOBAL_XACTS = 8
  val GLOBAL_XACT_ID_BITS = log2Up(NGLOBAL_XACTS)

}
