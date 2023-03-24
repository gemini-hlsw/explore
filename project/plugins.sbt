resolvers in Global ++= Resolver.sonatypeOssRepos("public")
resolvers in Global ++= Resolver.sonatypeOssRepos("snapshots")

addSbtPlugin("edu.gemini"   % "sbt-lucuma-app" % "0.10.11")
addSbtPlugin("edu.gemini"   % "sbt-lucuma-css" % "0.10.11")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"  % "0.11.0")
