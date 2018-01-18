name := "xml"

version := "0.2.0.3"
organization := "com.github.rgafiyatullin"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
scalacOptions ++= Seq("-language:implicitConversions")
scalacOptions ++= Seq("-Ywarn-value-discard", "-Xfatal-warnings")

publishTo := {
  Some("releases"  at "https://artifactory.wgdp.io:443/xmppcs-maven-releases/")
}
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials.wg-domain")

scalaVersion in ThisBuild := "2.12.4"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4"
  )
