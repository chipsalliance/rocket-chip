package uncore
import _root_.uncore.constants._

//TODO: When compiler bug SI-5604 is fixed in 2.10, remove object Constants and 
//      mixin Constants traits to package object uncore in package.scala and 
//      remove import Constants._'s from other .scala files
object Constants extends 
  MemoryOpConstants with
  MemoryInterfaceConstants with
  CacheConstants with
  AddressConstants
{
}
