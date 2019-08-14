// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import java.io._

import scala.collection.mutable

import scala.reflect.ClassTag
import scala.util.Try

// TODO
// Dont forget, file writes are NOT atomic, use a move
// Regular 32-bit built-in hashes can collide, should we use something like md5? sha3?
object CacheCompute {
  private def time[R](name: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val timeMillis = (t1 - t0) / 1000000.0
    println(s" ***** $name took $timeMillis ms *****")
    result
  }
  //val EnvVarName: String = "CACHE_COMPUTE"
  //val cacheDir: Option[File] = Option(System.getenv(EnvVarName)).map(new File(_))
  val cacheDir: Option[File] = Some(new File("cache"))
  cacheDir.foreach { dir =>
    if (!dir.exists) {
      println(s"Cache directory $dir does not exist. Creating...")
      dir.mkdirs()
    }
  }

  //def read[K <: Serializable](name: String)(key: K, hashCode: Option[Int] = None): Option[Any] = {
  //  val hc = hashCode.getOrElse(key.hashCode)
  //}

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

  def getCacheFile(hc: Int): Option[File] = cacheDir.map(dir => new File(dir, s"$hc.cacheobj"))

  def read[K <: Serializable : ClassTag, V <: Serializable : ClassTag](key: => K, hc: => Int): Option[V] = {
    time(s"Reading from cache $cacheDir") {
      getCacheFile(hc).filter(_.exists()).flatMap { file =>
        println("cacheobj file found")
        val istream = Try(new ObjectInputStream(new FileInputStream(file)))
        println(istream)
        println(istream.get.readObject())
        val iobj = istream.map(_.readObject()).toOption
        println(iobj)
        iobj match {
          case Some(value) => println(value)
          case _ =>
        }
        iobj.collect { case (k: K, value: V) if k == key => println("object match"); value }
      }
    }
  }
  // This writes unconditionally
  def write[K <: Serializable, V <: Serializable](key: => K, hc: => Int, value: => V): Unit = {
    time(s"Writing to $cacheDir") {
      getCacheFile(hc).foreach { file =>
        val ostream = Try(new ObjectOutputStream(new FileOutputStream(file)))
        ostream.map(_.writeObject((key, value)))
        ostream.map(_.close())
      }
    }
  }

  def diskCache[K <: Serializable : ClassTag, V <: Serializable : ClassTag](key: K, hc: Int, value: => V): V = {
    read[K, V](key, hc) match {
      case Some(cachedValue: V) =>
        println("Found in cache!")
        cachedValue
      case None =>
        val calcValue = value
        write(key, hc, calcValue)
        calcValue
    }
  }

  private val cache = collection.mutable.WeakHashMap.empty[Int, Any]

  def apply[K <: Serializable : ClassTag, V <: Serializable : ClassTag](key: K, f: (K) => V): V = apply(key, f(key))
  def apply[K <: Serializable : ClassTag, V <: Serializable : ClassTag](key: K, f: => V): V = time("CacheCompute") {
    return f
    val hc = key.hashCode
    cache.getOrElseUpdate(hc, diskCache(key, hc, f)) match {
      case value: V => value
      case other => throw new Exception("Unexpected Error!")
    }
  }

  //import 
  //private var cmcache: mutable.Map[java.util.List[java.util.List[AddressSet]], java.math.BigInteger] =
  //  ChronicleMap
  //    .of(classOf[String], classOf[java.util.List])
  //    .averageKey("key")
  //    .averageValue(...)
  //    .valueMarshaller(ListMarshaller.of(
  //        CharSequenceBytesReader.INSTANCE,
  //        CharSequenceBytesWriter.INSTANCE
  //    ))
  //    .entries(...)
  //    .createPersistedTo(file)

  //def addressSet(key: List[List[AddressSet]])
}
