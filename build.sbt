organization := "org.goldenport"

name := "goldenport-record"

version := "1.3.9"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Backlog releases" at "https://everforth.backlog.jp/dav/APC/maven/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "1.0.1"

// libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.43"

// libraryDependencies += "org.smartdox" %% "smartdox" % "1.2.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "compile"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3" % "compile"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1" % "compile"

libraryDependencies += "com.zaxxer" % "HikariCP-java7" % "2.4.13" % "compile"

libraryDependencies += "org.apache.poi" % "poi" % "3.12" % "compile"

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.12" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.1" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
// publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
AutoMkcol.globalSettings

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishTo <<= version { v: String =>
  val backlog = "https://everforth.backlog.jp/dav/APC/maven/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Backlog snapshots" at backlog + "snapshots")
  else
    Some("Backlog releases" at backlog + "releases")
}
