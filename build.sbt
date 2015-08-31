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
        libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0",
        libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.5.2"
    )

lazy val shadingJVM = shading.jvm
lazy val shadingJS = shading.js
