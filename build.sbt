name := "stapl-core"

organization := "stapl"

description := "Simple Tree-structured Attribute-based Policy Language"

scalaVersion := "2.11.6"

scalacOptions += "-feature"

version := "0.1-experimental"

libraryDependencies ++= Seq("junit" % "junit" % "4.11" % "test",
                            "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test",
                            "joda-time" % "joda-time" % "2.2",
                            "org.joda" % "joda-convert" % "1.7",
                            "org.scala-lang" % "scala-compiler" % "2.11.5",
                            "ch.qos.logback" % "logback-classic" % "1.1.2",
                            "org.clapper" % "grizzled-slf4j_2.11" % "1.0.2",
                            "org.clapper" % "grizzled-scala_2.11" % "1.2",
                            "org.scala-lang.modules" % "scala-swing_2.11" % "1.0.1")

import de.heikoseeberger.sbtheader.license.Apache2_0

headers := Map(
  "scala" -> Apache2_0("2015", "Jasper Moeys, iMinds-DistriNet, KU Leuven")
)
