enablePlugins(ScalaJSPlugin)

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"

scalaJSUseMainModuleInitializer := true
