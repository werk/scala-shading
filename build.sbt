name := "Scala Sharing Root Project"

lazy val root = project.in(file("."))
  .aggregate(projectJS, projectJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

// Project containing source code shared between the JS and JVM projects.
// This project should never be compiled or packages but is simply an IntelliJ IDEA
// friendly alternative to a shared code directory. Projects depending on this
// projects source code should declare a dependency as 'Provided' AND append
// this projects source directory manually to 'unmanagedSourceDirectories'.
lazy val projectShared = project.in(file("shared"))

lazy val projectSharedSettings = Seq(
  name := "scala-shading",
  version := "0.1-SNAPSHOT",
  // NOTE: The following line will generate a warning in IntelliJ IDEA, which can be ignored:
  // "The following source roots are outside the corresponding base directories"
  unmanagedSourceDirectories in Compile += (scalaSource in (projectShared, Compile)).value
)

lazy val projectJS = project.in(file("js"))
  .dependsOn(projectShared % Provided)
  .settings(scalaJSSettings: _*)
  .settings(projectSharedSettings: _*)
  .settings(
    // Add JS-specific settings here
  )

lazy val projectJVM = project.in(file("jvm"))
  .dependsOn(projectShared % Provided)
  .settings(projectSharedSettings: _*)
  .settings(
    // Add JVM-specific settings here
  )
