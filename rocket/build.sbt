organization := "edu.berkeley.cs"

version := "1.2"

name := "rocket"

scalaVersion := "2.11.6"

libraryDependencies ++= (Seq("chisel", "hardfloat", "uncore", "junctions", "cde").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten
