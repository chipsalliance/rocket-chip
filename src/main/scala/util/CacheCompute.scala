// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import java.io._

import scala.collection.mutable

import scala.reflect.ClassTag

object CacheCompute {
  //val EnvVarName: String = "CACHE_COMPUTE"
  //val cacheDir: Option[File] = Option(System.getenv(EnvVarName)).map(new File(_))

  ////def read[K <: Serializable](name: String)(key: K, hashCode: Option[Int] = None): Option[Any] = {
  ////  val hc = hashCode.getOrElse(key.hashCode)
  ////}

  //def apply[K, V <: Serializable](key: K, f: (K) => V): V = {
  //  cacheDir.map { dir =>
  //    val hashCode = x.hashCode.toHexString
  //    val file = new File(dir, s"$hashCode.key")
  //    val ostream = new ObjectOutputStream(new FileOutputStream(file))
  //    ostream.writeObject(x)
  //    ostream.close()
  //    true

  //    f(key)
  //  }.getOrElse
  //}
  private val cache = collection.mutable.WeakHashMap.empty[Any, Any]

  def apply[K, V : ClassTag](key: K, f: (K) => V): V = apply(key, f(key))
  def apply[K, V : ClassTag](key: K, f: => V): V = {
    cache.getOrElseUpdate(key, f) match {
      case value: V => value
      case other => throw new Exception("Unexpected Error!")
    }
  }
}
