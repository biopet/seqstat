organization := "com.github.biopet"
organizationName := "Biopet"

startYear := Some(2014)

name := "SeqStat"
biopetUrlName := "seqstat"

biopetIsTool := true

mainClass in assembly := Some("nl.biopet.tools.seqstat.SeqStat")

developers := List(
  Developer(id = "ffinfo",
            name = "Peter van 't Hof",
            email = "pjrvanthof@gmail.com",
            url = url("https://github.com/ffinfo"))
)

crossScalaVersions := Seq("2.11.12", "2.12.5")

scalaVersion := "2.11.12"

libraryDependencies += "com.github.biopet" %% "common-utils" % "0.7"
libraryDependencies += "com.github.biopet" %% "tool-utils" % "0.5"
libraryDependencies += "com.github.biopet" %% "tool-test-utils" % "0.2.2" % Test
libraryDependencies += "com.github.biopet" %% "ngs-utils" % "0.5"
