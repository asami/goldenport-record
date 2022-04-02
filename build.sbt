organization := "org.goldenport"

name := "goldenport-record"

version := "1.3.52"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2019" at "https://raw.github.com/asami/maven-repository/2019/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases 2021" at "https://raw.github.com/asami/maven-repository/2021/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "1.0.1"

// libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.3.20"

// libraryDependencies += "org.smartdox" %% "smartdox" % "1.2.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "compile"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3" % "compile"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "compile"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1" % "compile"

libraryDependencies += "com.zaxxer" % "HikariCP-java7" % "2.4.13" % "compile"

libraryDependencies += "org.apache.poi" % "poi" % "3.12" % "compile"

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.12" % "compile"

libraryDependencies += "org.jfree" % "jfreechart" % "1.5.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.1" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}
