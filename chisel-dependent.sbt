// Provide a managed dependency on chisel if -DchiselVersion="" is
// supplied on the command line.

lazy val chiselVersion = System.getProperty("chiselVersion", "None")

libraryDependencies ++= ( if (chiselVersion != "None" ) (
    "edu.berkeley.cs" %% "chisel" % chiselVersion
) :: Nil; else Nil)
