// See README.md for license details.
def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

name := "creec"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.12", "2.12.4")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.2-SNAPSHOT",
  "chisel-iotesters" -> "1.2.+",
)

libraryDependencies ++= (Seq("chisel3", "chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })

libraryDependencies ++= Seq("org.typelevel" %% "squants" % "1.3.0")
libraryDependencies ++= Seq("com.lihaoyi" %% "utest" % "0.6.5")
libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel-testers2" % "0.1-SNAPSHOT")

scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
scalacOptions ++= Seq("-language:reflectiveCalls", "-unchecked", "-deprecation", "-feature")

javacOptions ++= javacOptionsVersion(scalaVersion.value)
cancelable in Global := true

libraryDependencies ++= Seq("commons-codec" % "commons-codec" % "1.9")

libraryDependencies ++= Seq("edu.berkeley.cs" %% "rocket-dsptools" % "1.2-102318-SNAPSHOT")

