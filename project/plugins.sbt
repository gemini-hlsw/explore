resolvers in Global ++= Resolver.sonatypeOssRepos("public")
resolvers in Global ++= Resolver.sonatypeOssRepos("snapshots")

addSbtPlugin("edu.gemini"   % "sbt-lucuma-app" % "0.10.6")
addSbtPlugin("edu.gemini"   % "sbt-lucuma-css" % "0.10.6")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"  % "0.11.0")
