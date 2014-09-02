// Provide a managed dependency on chisel if -DchiselVersion="" is
// supplied on the command line.

val chiselVersion_r = System.getProperty("chiselVersion", "None")

// _r a temporary fix until sbt 13.6 https://github.com/sbt/sbt/issues/1465

libraryDependencies ++= ( if (chiselVersion_r != "None" ) (
    "edu.berkeley.cs" %% "chisel" % chiselVersion_r
) :: Nil; else Nil)
