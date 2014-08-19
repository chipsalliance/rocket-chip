// Provide a managed dependency on chisel if -DhardfloatVersion="" is
// supplied on the command line.

val hardfloatVersion = System.getProperty("hardfloatVersion", "None")

libraryDependencies ++= ( if (hardfloatVersion != "None" ) (
    "edu.berkeley.cs" %% "hardfloat" % hardfloatVersion
) :: Nil; else Nil)
