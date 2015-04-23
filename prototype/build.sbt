name := "interpolation"

organization := "io.github.dzufferey"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "io.github.dzufferey" %% "scala-arg" % "0.1-SNAPSHOT",
  "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT",
  "io.github.dzufferey" %% "scala-smtlib-interface" % "0.1-SNAPSHOT"
)

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

