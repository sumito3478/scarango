// Copyright 2014 sumito3478 <sumito3478@gmail.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Scalariform Settings
lazy val scalariformSettings = {
  import scalariform.formatter.preferences._
  import com.typesafe.sbt.SbtScalariform
  SbtScalariform.scalariformSettings ++ Seq(
    SbtScalariform.ScalariformKeys.preferences := FormattingPreferences()
      .setPreference(DoubleIndentClassDeclaration, true))
}

// sbt-release settings
lazy val releaseSettings = {
  import sbtrelease.ReleasePlugin.ReleaseKeys._
  sbtrelease.ReleasePlugin.releaseSettings ++ Seq(
    crossBuild := true,
    tagComment <<= (version in ThisBuild) map (v => s"Release $v"),
    commitMessage <<= (version in ThisBuild) map (v => s"Bump version number to $v"))
}

lazy val macroParadiseSettings = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
)

lazy val buildInfoSettings = {
  import sbtbuildinfo.Plugin._
  sbtbuildinfo.Plugin.buildInfoSettings ++ Seq(
    sourceGenerators in Compile += buildInfo.taskValue,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "scarango"
  )
}

lazy val commonSettings = Seq(
  javaOptions := Seq("-Xms1024m"),
  organization := "info.sumito3478",
  scalaVersion := "2.11.2",
  crossScalaVersions := Seq("2.10.4", "2.11.2"),
  fork := true,
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
  libraryDependencies <+= scalaVersion {
    v =>
      val shapeless = "com.chuusai" %% "shapeless" % "2.0.0"
      if (v.startsWith("2.10")) shapeless cross CrossVersion.full else shapeless
  },
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "ch.qos.logback" % "logback-classic" % "1.1.2" % "test",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.4.2",
    "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.2",
    "com.ning" % "async-http-client" % "1.8.13",
    "org.scalatest" %% "scalatest" % "2.2.2" % "test"
  ),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalacOptions ++= Seq(
    "-encoding", "utf-8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xexperimental",
    "-Xcheckinit",
    "-Xlint")) ++ scalariformSettings ++ releaseSettings ++ macroParadiseSettings

lazy val commonConfiguration: Project => Project =
  p => p.copy(id = s"scarango-${p.id}") // prefix project name with scarango
    .copy(base = file(p.base.getName.split('-').mkString("/"))) // convert project dir from a-b-c to a/b/c

// Aggregate and depends on all modules
lazy val all = (project in file(".")).configure(commonConfiguration).aggregate(core).dependsOn(core).settings(commonSettings: _*)

// core module
lazy val core = project.configure(commonConfiguration).settings(commonSettings: _*).settings(buildInfoSettings: _*)
