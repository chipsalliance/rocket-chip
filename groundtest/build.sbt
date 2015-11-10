organization := "edu.berkeley.cs"

version := "1.0"

name := "groundtest"

scalaVersion := "2.11.6"

libraryDependencies ++= (Seq("chisel", "uncore", "junctions", "rocket").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten
