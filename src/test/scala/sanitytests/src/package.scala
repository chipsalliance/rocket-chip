import os._

package object sanitytests {
  def resource(file: String): Path = Path(java.nio.file.Paths.get(getClass().getClassLoader().getResource(file).toURI))
  def resourceOut(file: String): String = pwd.toString() + "/out/rocketchip/sanitytests/libraryResources.dest/" + file
}
