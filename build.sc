import mill._
import mill.define.Target
import scalalib._

import ammonite.ops._
import ammonite.ops.ImplicitWd._
import mill.scalalib._
import mill.scalalib.publish._
import mill.eval.Evaluator

def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    // Using -Xsource:2.11 to work around scala compiler "regression" involving anonymous Bundle reflective access
    // See https://github.com/freechipsproject/chisel3/issues/606
    if (scalaVersion.startsWith("2.11.")) {
      Seq()
    } else {
      Seq(
        "-Xsource:2.11",
        "-Ywarn-unused:imports",
        "-Ywarn-unused:locals"
      )
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    if (scalaVersion.startsWith("2.11.")) {
      Seq("-source", "1.7", "-target", "1.7")
    } else {
      Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

// An sbt layout with src in the top directory.
// Extending CrossSbtModule tells mill our source paths look like <project>/src/main/scala
trait CrossUnRootedSbtModule extends CrossSbtModule {
  // We actually have our source paths looking like /src/main/scala without <project> so we need this too
  override def millSourcePath = super.millSourcePath / ammonite.ops.up
}

trait CommonModule extends CrossUnRootedSbtModule with PublishModule {
  def publishVersion = "0.0.1-SNAPSHOT"

  override def scalacOptions = scalacOptionsVersion(crossScalaVersion) ++ Seq(
    "-deprecation",
    "-explaintypes",
    "-feature", "-language:reflectiveCalls",
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator"
  )

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/ucberkeley-ee290c/fa18-smartnic.git",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("ucberkeley-ee290c", "fa18-smartnic"),
    developers = Seq(
      Developer("vighneshiyer", "Vighnesh Iyer", "https://github.com/vighneshiyer/")
    )
  )

  override def javacOptions = javacOptionsVersion(crossScalaVersion)
}

val crossVersions = Seq("2.12.4", "2.11.12")

// Make this available to external tools.
object creec extends Cross[CREECModule](crossVersions: _*) {
  //def scalaVersion = "2.12.4"
  def defaultVersion(ev: Evaluator) = T.command{
    println(crossVersions.head)
  }

  def compile = T{
    creec(crossVersions.head).compile()
  }

  def jar = T{
    creec(crossVersions.head).jar()
  }

  def test = T{
    creec(crossVersions.head).test.test()
  }

  def publishLocal = T{
    creec(crossVersions.head).publishLocal()
  }

  def docJar = T{
    creec(crossVersions.head).docJar()
  }
}

class CREECModule(val crossScalaVersion: String) extends CommonModule {
  override def artifactName = "creec"

  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.2-SNAPSHOT",
    ivy"edu.berkeley.cs::chisel-iotesters:1.2.+",
    ivy"org.typelevel::squants:1.3.0"
  )

  // uTest works great with mill!
  // The issue with scalatest is you can't only run a single test suite or test from the mill command line
  // You have to use an ugly REPL hack, and this is the fault of scalatest's main runner API and its tight SBT integration
  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.5"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
  override def mainClass = Some("chisel3.Driver")
}
