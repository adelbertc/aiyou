import UnidocKeys._
import ReleaseTransformations._

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
	),
  libraryDependencies ++= List(
    "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
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
  .dependsOn(core_cats, core_scalaz)
  .aggregate(core_cats, core_scalaz)

lazy val core_cats = project.in(file("core-cats"))
  .settings(
    name := "core-cats",
    yax(file("yax/core"), "cats"),
    buildSettings ++ commonSettings ++ publishSettings,
    libraryDependencies ++= List(
      "org.typelevel" %% "cats" % "0.6.1"
    )
  )

val scalazVersion = "7.2.4"

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
