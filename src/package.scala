package uncore
import uncore.constants._

//TODO: When compiler bug SI-5604 is fixed in 2.10, change object Constants to 
//      package object uncore and remove import Constants._'s from other files
object Constants extends 
  MemoryOpConstants with
  MemoryInterfaceConstants with
  CacheConstants with
  AddressConstants
{
}

