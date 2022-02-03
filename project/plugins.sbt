resolvers in Global += Resolver.sonatypeRepo("public")
resolvers in Global += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("edu.gemini"       % "sbt-lucuma-app" % "0.6-3ce8fc2-SNAPSHOT")
addSbtPlugin("com.eed3si9n"     % "sbt-buildinfo"  % "0.10.0")
addSbtPlugin("com.timushev.sbt" % "sbt-rewarn"     % "0.1.3")
