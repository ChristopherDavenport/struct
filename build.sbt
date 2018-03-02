lazy val struct = project.in(file("."))
    .settings(commonSettings)
    .settings(
      name := "struct"
    )

/***********************************************************************\
                      Boilerplate below these lines
\***********************************************************************/




lazy val commonSettings = Seq(
  organization := "io.chrisdavenport",
  licenses += ("MIT", url("https://opensource.org/licenses/MIT")),

  scalaVersion := "2.12.4",

  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.5" cross CrossVersion.binary),

  libraryDependencies ++= Seq(
    "org.typelevel"               %% "cats-core"                  % "1.0.1",
    "org.typelevel"               %% "cats-testkit"               % "1.0.1",
  )
)
