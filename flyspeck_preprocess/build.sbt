name := "flyspec-preprocess"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "io.github.dzufferey" %% "s-expr" % "0.1-SNAPSHOT",
  "io.github.dzufferey" %% "scala-arg" % "0.1-SNAPSHOT",
  "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT"
)

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

