package rocket
package config

import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scala.util.{Properties => SProperties}

class Config(props: Properties) {
  private val msg = "Configuration is missing requested parameter "
  def getInt(name: String): Int = Option(props.getProperty(name).toInt).getOrElse(sys.error(msg+name))
  def getString(name: String): String = Option(props.getProperty(name)).getOrElse(sys.error(msg+name))
  def getBoolean(name: String): Boolean = Option(props.getProperty(name).toBoolean).getOrElse(sys.error(msg+name))
  def apply(name: String): Int = getInt(name)
}

object Config {

  lazy val internal_config = getConfig()

  def apply(name: String) = internal_config(name)

  private def getConfig(): Config = {

    val filePath0 = 
      SProperties
        .envOrNone("ROCKET_CONFIG")
        .orElse(SProperties.propOrNone("rocket.config"))  
    if (filePath0.isEmpty)
      Console.err.println("""
      | WARNING: Could not find configuration file to load. 
      | Options are:
      |   (1) Set environmental variable ROCKET_CONFIG to the config file path
      |   (2) Set system property rocket.config to the config file path
      | Using default values for config.
      """.stripMargin)

    val filePath =
      filePath0.flatMap(fp => {
        val f = new File(fp)
        if (!f.isFile) {
          Console.err.println("""
          | WARNING: File '%s' is not a valid file path
          | Using default values for config
          """.format(fp).stripMargin)
          None
        } else Some(fp)
      })

    val props = new Properties()
    filePath.map(fp => props.load(new FileInputStream(fp)))
    new Config(props)
  }

}

