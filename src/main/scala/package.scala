package rocket
import rocket.constants._

import Chisel._
import scala.math._

//TODO: When compiler bug SI-5604 is fixed in 2.10, change object Constants to 
//      package object rocket and remove import Constants._'s from other files
object Constants extends 
  ScalarOpConstants with
  uncore.constants.MemoryOpConstants with
  PCRConstants with 
  InterruptConstants with 
  RocketDcacheConstants with
  VectorOpConstants with 
  TLBConstants with 
  uncore.constants.MemoryInterfaceConstants
{
  def HAVE_RVC = false
  def HAVE_FPU = true
  def HAVE_VEC = true

  val MAX_THREADS = 
      hwacha.Constants.NUM_PVFB * hwacha.Constants.WIDTH_PVFB / hwacha.Constants.SZ_BANK

  val START_ADDR = 0x2000

}
