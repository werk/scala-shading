lazy val root = project.in(file(".")).
    aggregate(shadingJS, shadingJVM).
    settings(
        publish := {},
        publishM2 := {},
        publishLocal := {}
    )

lazy val shading = crossProject.in(file(".")).
    settings(
        organization := "dk.mzw",
        name := "accelemation",
        version := "1.0.0-SNAPSHOT",
        scalaVersion := "2.12.1"
    ).
    jvmSettings(
    ).
    jsSettings(
        //libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
    )

lazy val shadingJVM = shading.jvm
lazy val shadingJS = shading.js
