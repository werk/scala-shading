name := "Scala Sharing Root Project"

lazy val root = project.in(file(".")).
  aggregate(shadingJS, shadingJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val shading = crossProject.in(file(".")).
  settings(
    name := "scala-shading",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.6"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val shadingJVM = shading.jvm
lazy val shadingJS = shading.js
