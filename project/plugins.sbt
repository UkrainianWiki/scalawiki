logLevel := Level.Warn

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.1")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.6.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
