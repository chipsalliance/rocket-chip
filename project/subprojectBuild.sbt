import chiselBuild.ChiselDependencies.subProjectsSetting

lazy val root = project.in(file(".")).settings(
  sourceGenerators in Compile += Def.task {
    SubprojectGenerator.generate((sourceManaged in Compile).value, subProjectsSetting.value)
  }.taskValue
)
