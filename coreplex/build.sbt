organization := "edu.berkeley.cs"

version := "1.0"

name := "coreplex"

scalaVersion := "2.11.6"

libraryDependencies ++= (Seq("chisel", "uncore", "junctions", "rocket", "groundtest").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten
