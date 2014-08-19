// Provide a managed dependency on chisel if -DuncoreVersion="" is
// supplied on the command line.

val uncoreVersion = System.getProperty("uncoreVersion", "None")

libraryDependencies ++= ( if (uncoreVersion != "None" ) (
    "edu.berkeley.cs" %% "uncore" % uncoreVersion
) :: Nil; else Nil)
