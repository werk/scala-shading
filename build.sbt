name := "Scala Shading Root Project"

lazy val root = project.in(file(".")).
    aggregate(shadingJS, shadingJVM).
    settings(
        publish := {},
        publishLocal := {}
    )

lazy val shading = crossProject.in(file(".")).
    settings(
        name := "scala-shading",
        version := "0.1.0",
        scalaVersion := "2.12.1"
    ).
    jvmSettings(
        // Add JVM-specific settings here
    ).
    jsSettings(
        //libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"
    )

lazy val shadingJVM = shading.jvm
lazy val shadingJS = shading.js
