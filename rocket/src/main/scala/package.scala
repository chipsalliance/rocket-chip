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
  val START_ADDR = 0x2000
}
