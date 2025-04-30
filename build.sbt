
val toolkitV = "0.5.0"
val toolkit = "org.scala-lang" %% "toolkit" % toolkitV
val toolkitTest = "org.scala-lang" %% "toolkit-test" % toolkitV

ThisBuild / scalaVersion := "3.3.4"
libraryDependencies += toolkit
libraryDependencies += (toolkitTest % Test)
libraryDependencies += "org.rogach" %% "scallop" % "5.2.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.10.0"
