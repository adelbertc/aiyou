import UnidocKeys._
import ReleaseTransformations._

val catsVersion = "0.6.1"
val scalazVersion = "7.2.4"

lazy val buildSettings = List(
  organization := "org.tpolecat",
  licenses ++= List(
    ("MIT", url("http://opensource.org/licenses/MIT")),
    ("BSD New", url("http://opensource.org/licenses/BSD-3-Clause"))
  ),
  scalaVersion := "2.11.8",
  crossScalaVersions := List("2.10.6", scalaVersion.value),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)
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
    "-Ywarn-value-discard"
  ),
  scalacOptions in compile ++= List(
    "-Yno-imports",
    "-Ywarn-numeric-widen"
  )
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
  homepage := Some(url("https://github.com/tpolecat/io")),
  pomIncludeRepository := Function.const(false),
  pomExtra := (
    <scm>
      <url>git@github.com:tpolecat/tut.git</url>
      <connection>scm:git:git@github.com:tpolecat/tut.git</connection>
    </scm>
    <developers>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>http://tpolecat.org</url>
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

lazy val io = project.in(file("."))
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

lazy val tests_cats = project.in(file("tests-cats"))
  .dependsOn(core_cats, laws_cats)
  .settings(
    yax(file("yax/tests"), "cats"),
    buildSettings ++ commonSettings,
    libraryDependencies ++= List(
      "org.typelevel"   %% "discipline"         % "0.5"     % "test",
      "org.specs2"      %% "specs2-core"        % "3.6.6"   % "test",
      "org.specs2"      %% "specs2-scalacheck"  % "3.6.6"   % "test"
    )
  )

lazy val core_scalaz = project.in(file("core-scalaz"))
  .settings(
    name := "core-scalaz",
    yax(file("yax/core"), "scalaz"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.scalaz"  %% "scalaz-core"    % scalazVersion,
      "org.scalaz"  %% "scalaz-effect"  % scalazVersion
    )
  )

lazy val laws_scalaz = project.in(file("laws-scalaz"))
  .dependsOn(core_scalaz)
  .settings(
    name := "laws-scalaz",
    yax(file("yax/laws"), "scalaz"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.scalaz"      %% "scalaz-scalacheck-binding"  % scalazVersion,
      "org.scalacheck"  %% "scalacheck"                 % "1.12.5"
    )
  )

lazy val tests_scalaz = project.in(file("tests-scalaz"))
  .dependsOn(core_scalaz, laws_scalaz)
  .settings(
    yax(file("yax/tests"), "scalaz"),
    buildSettings ++ commonSettings,
    libraryDependencies ++= List(
      "org.specs2"  %% "specs2-core"        % "3.7"  % "test",
      "org.specs2"  %% "specs2-scalacheck"  % "3.7"  % "test"
    )
  )
