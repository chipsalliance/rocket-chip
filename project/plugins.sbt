resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")

/* Prevent
    SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder"
    SLF4J: Defaulting to no-operation (NOP) logger implementation
   due to the sbt-git plugin
 */
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.7"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
