Uncore Library
==============

This is the repository for uncore components assosciated with Rocket chip
project. To uses these modules, include this repo as a git submodule within
the your chip repository and add it as Project in your chip's build.scala. 
These components are only dependent on Chisel, i.e.

    lazy val uncore = Project("uncore", file("uncore"), settings = buildSettings) dependsOn(chisel)

Documentation about the uncore library will come in the near future.
