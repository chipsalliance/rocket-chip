package freechips.rocketchip.resources

import freechips.rocketchip.diplomacy.{AddressRange}

case class AddressMapEntry(range: AddressRange, permissions: ResourcePermissions, names: Seq[String]) {
  val ResourcePermissions(r, w, x, c, a) = permissions

  def toString(aw: Int) = s"\t%${aw}x - %${aw}x %c%c%c%c%c %s".format(
    range.base,
    range.base+range.size,
    if (a) 'A' else ' ',
    if (r) 'R' else ' ',
    if (w) 'W' else ' ',
    if (x) 'X' else ' ',
    if (c) 'C' else ' ',
    names.mkString(", "))

  def toJSON = s"""{"base":[${range.base}],"size":[${range.size}],""" +
    s""""r":[$r],"w":[$w],"x":[$x],"c":[$c],"a":[$a],""" +
    s""""names":[${names.map(n => s""""$n"""").mkString(",")}]}"""
}
