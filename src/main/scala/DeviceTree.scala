package rocketchip

case object DeviceTree extends cde.Field[Array[Byte]]

private class StringTable {
  private val strings = collection.mutable.HashMap[String, Int]()
  private val data = collection.mutable.ArrayBuffer[Byte]()

  def add(x: String) = {
    if (!strings.contains(x)) {
      strings(x) = data.length
      data ++= x.getBytes
      data += 0
    }
    strings(x)
  }

  def toArray = data.toArray
}

class DeviceTreeGenerator {
  def beginNode(name: String): Unit = {
    append(pack(1))
    append(pack(name))
  }
  def endNode(): Unit = append(pack(2))
  def addProp(name: String, data: Int): Unit = addProp(name, pack(data), 4)
  def addProp(name: String, data: String): Unit =
    addProp(name, pack(data), data.getBytes.length+1)
  def addReg(values: Long*): Unit = {
    val seq = values.toSeq
    val buf = java.nio.ByteBuffer.allocate(seq.length*8)
    seq foreach buf.putLong
    addProp("reg", buf.array, buf.array.length)
  }
  def toArray(): Array[Byte] = {
    append(pack(9))
    val structArray = os.toByteArray
    val stringArray = strings.toArray

    val headerSize = 40
    val rsvMap = Array.fill[Byte](16)(0)
    val rsvMapOffset = headerSize
    val structOffset = headerSize + rsvMap.length
    val stringOffset = structOffset + structArray.length
    val totalSize = stringOffset + stringArray.length

    os.reset()
    append(pack(0xd00dfeed)) // magic
    append(pack(totalSize))
    append(pack(structOffset))
    append(pack(stringOffset))
    append(pack(rsvMapOffset))
    append(pack(17)) // version
    append(pack(16)) // compatible version
    append(pack(0)) // boot cpuid
    append(pack(stringArray.length))
    append(pack(structArray.length))
    append(rsvMap)
    append(structArray)
    append(stringArray)

    val res = os.toByteArray
    os.reset
    res
  }

  private val os = new java.io.ByteArrayOutputStream
  private val strings = new StringTable
  private def pack(x: String) = x.getBytes.padTo((x.getBytes.length+4)/4*4, 0.toByte)
  private def pack(x: Int) = java.nio.ByteBuffer.allocate(4).putInt(x).array
  private def append(x: Array[Byte]) = os.write(x, 0, x.length)
  private def addProp(name: String, data: Array[Byte], length: Int) = {
    require(data.length % 4 == 0)
    append(pack(3))
    append(pack(length))
    append(pack(strings.add(name)))
    append(data)
  }
}
