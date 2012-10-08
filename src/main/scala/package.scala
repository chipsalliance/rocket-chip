package rocket
import rocket.constants._

import Chisel._
import scala.math._

//TODO: When compiler bug SI-5604 is fixed in 2.10, change object Constants to 
//      package object rocket and remove import Constants._'s from other files
object Constants extends HTIFConstants with
  MemoryOpConstants with
  PCRConstants with 
  InterruptConstants with 
  AddressConstants with
  ArbiterConstants with 
  VectorOpConstants with 
  TLBConstants with 
  ScalarOpConstants with
  MemoryInterfaceConstants
{
  val HAVE_RVC = false
  val HAVE_FPU = true
  val HAVE_VEC = true

  val MAX_THREADS = 
      hwacha.Constants.NUM_PVFB * hwacha.Constants.WIDTH_PVFB / hwacha.Constants.SZ_BANK

  val START_ADDR = 0x2000

}
