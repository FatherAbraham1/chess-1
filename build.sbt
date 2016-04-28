name := "Chess"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.5.0" % "provided"

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)
