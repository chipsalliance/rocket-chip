package object uncore extends
  uncore.constants.MemoryOpConstants with
  uncore.constants.MemoryInterfaceConstants with
  uncore.constants.CacheConstants with
  uncore.constants.AddressConstants 
{
  implicit def toOption[A](a: A) = Option(a)
}
