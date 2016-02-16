logLevel := Level.Warn

resolvers += Classpaths.sbtPluginReleases

// Plugin for scoverage:
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

// Plugin for publishing scoverage results to coveralls:
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.0.3")
