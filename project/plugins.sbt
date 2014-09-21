logLevel := Level.Warn

resolvers += "Typesafe Repository (Releases)" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.0.1")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")