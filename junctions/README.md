# junctions
A repository for peripheral components and IO devices associated with the RocketChip project.

To uses these modules, include this repo as a git submodule within the your chip repository and add it as Project in your chip's build.scala. These components are only dependent on Chisel, i.e.

    lazy val junctions = project.dependsOn(chisel)
