import UnidocKeys._
import ReleaseTransformations._

val catsVersion = "0.8.1"
val scalazVersion = "7.2.7"

val disabledReplOptions = Set("-Ywarn-unused-import")

lazy val buildSettings = List(
  organization := "io.github.adelbertc",
  licenses ++= List(
    ("MIT", url("http://opensource.org/licenses/MIT")),
    ("BSD New", url("http://opensource.org/licenses/BSD-3-Clause"))
  ),
  scalaVersion := "2.12.0",
  crossScalaVersions := List("2.10.6", "2.11.8", scalaVersion.value),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
)

lazy val commonSettings = List(
  scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ scalaVersionFlags(scalaVersion.value),
  scalacOptions in (Compile, console) ~= { _.filterNot(disabledReplOptions.contains(_)) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  libraryDependencies ++= scalaVersionDeps(scalaVersion.value)
)

lazy val publishSettings = List(
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/adelbertc/aiyou")),
  pomIncludeRepository := Function.const(false),
  pomExtra := (
    <scm>
      <url>git@github.com:adelbert/aiyou.git</url>
      <connection>scm:git:git@github.com:adelbertc/aiyou.git</connection>
    </scm>
    <developers>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>http://tpolecat.org</url>
      </developer>
      <developer>
        <id>adelbertc</id>
        <name>Adelbert Chang</name>
        <url>https://github.com/adelbertc/</url>
      </developer>
    </developers>
  ),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := List[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    ReleaseStep(action = Command.process("package", _)),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges)
)

lazy val noPublishSettings = List(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val aiyou = project.in(file("."))
  .settings(buildSettings ++ commonSettings)
  .settings(noPublishSettings)
  .settings(unidocSettings)
  .dependsOn(core_cats, laws_cats, tests_cats, core_scalaz, laws_scalaz, tests_scalaz)
  .aggregate(core_cats, laws_cats, tests_cats, core_scalaz, laws_scalaz, tests_scalaz)

lazy val core_cats = project.in(file("core-cats"))
  .settings(
    name := "core-cats",
    yax(file("yax/core"), "cats"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % catsVersion
    )
  )

lazy val laws_cats = project.in(file("laws-cats"))
  .dependsOn(core_cats)
  .settings(
    name := "laws-cats",
    yax(file("yax/laws"), "cats"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.typelevel"   %% "cats-laws"  % catsVersion,
      "org.scalacheck"  %% "scalacheck" % "1.12.5"
    )
  )

val specs2Version = "3.8.6"

lazy val tests_cats = project.in(file("tests-cats"))
  .dependsOn(core_cats, laws_cats)
  .settings(
    yax(file("yax/tests"), "cats"),
    buildSettings ++ commonSettings,
    libraryDependencies ++= List(
      "org.typelevel"   %% "discipline"         % "0.7.1"       % "test",
      "org.specs2"      %% "specs2-core"        % specs2Version % "test",
      "org.specs2"      %% "specs2-scalacheck"  % specs2Version % "test"
    )
  )

lazy val core_scalaz = project.in(file("core-scalaz"))
  .settings(
    name := "core-scalaz",
    yax(file("yax/core"), "scalaz"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.scalaz"  %% "scalaz-core"  % scalazVersion
    )
  )

lazy val laws_scalaz = project.in(file("laws-scalaz"))
  .dependsOn(core_scalaz)
  .settings(
    name := "laws-scalaz",
    yax(file("yax/laws"), "scalaz"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.scalaz"      %% "scalaz-scalacheck-binding" % (scalazVersion ++ "-scalacheck-1.13"),
      "org.scalacheck"  %% "scalacheck"                % "1.13.4"
    )
  )

lazy val tests_scalaz = project.in(file("tests-scalaz"))
  .dependsOn(core_scalaz, laws_scalaz)
  .settings(
    yax(file("yax/tests"), "scalaz"),
    buildSettings ++ commonSettings,
    libraryDependencies ++= List(
      "org.specs2"  %% "specs2-core"        % specs2Version  % "test",
      "org.specs2"  %% "specs2-scalacheck"  % specs2Version  % "test"
    )
  )


def scalaVersionFlags(version: String): List[String] = CrossVersion.partialVersion(version) match {
  case Some((2, 10)) => List.empty
  case Some((2, 11)) => List("-Ywarn-unused-import")
  case Some((2, 12)) => List("-Ypartial-unification", "-Ywarn-unused-import")
  case _             => List.empty
}

def scalaVersionDeps(version: String): List[ModuleID] = CrossVersion.partialVersion(version) match {
  case Some((2, 10)) => List(compilerPlugin("com.milessabin" % "si2712fix-plugin_2.10.6" % "1.2.0"))
  case Some((2, 11)) => List(compilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"))
  case Some((2, 12)) => List.empty
  case _             => List.empty
}
