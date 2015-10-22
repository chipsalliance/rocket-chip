organization := "edu.berkeley.cs"

version := "1.0"

name := "junctions"

scalaVersion := "2.11.6"

// Provide a managed dependency on chisel if -DchiselVersion="" is supplied on the command line.
libraryDependencies ++= (Seq("chisel","cde").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:ucb-bar/junctions.git"
