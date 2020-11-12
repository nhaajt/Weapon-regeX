lazy val WeaponRegeX = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    scalaVersion := "2.13.3",
    crossScalaVersions := List("2.13.3", "2.12.12"),
    name := "weapon-regex",
    version := "0.1",
    // libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.6.0",
    // libraryDependencies += "com.kyleu" %% "reftree" % "1.4.1", // Unofficial fork that works with Scala 2.13
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.16" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided"
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    scalaJSUseMainModuleInitializer := true
  )
