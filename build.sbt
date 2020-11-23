// Skip publish root
skip in publish := true

inThisBuild(
  List(
    version := "0.1.0-SNAPSHOT",
    organization := "io.stryker-mutator",
    description := "A regular expresion mutator library",
    homepage := Some(url("https://github.com/Nhaajt/Weapon-regeX")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List()
  )
)

lazy val WeaponRegeX = projectMatrix
  .in(file("shared"))
  .settings(
    name := "weapon-regex",
    // libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.6.0",
    // libraryDependencies += "com.kyleu" %% "reftree" % "1.4.1", // Unofficial fork that works with Scala 2.13
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.16" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jvmPlatform(
    scalaVersions = List("2.13.3", "2.12.12"),
    settings = Seq(
      // Add JVM-specific settings here
      libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided"
    )
  )
  .jsPlatform(
    scalaVersions = List("2.13.3", "2.12.12"),
    settings = Seq(
      // Add JS-specific settings here
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    )
  )
