resolvers in Global ++= Resolver.sonatypeOssRepos("public")
resolvers in Global ++= Resolver.sonatypeOssRepos("snapshots")

addSbtPlugin("edu.gemini"   % "sbt-lucuma-app" % Versions.sbtLucuma)
addSbtPlugin("edu.gemini"   % "sbt-lucuma-css" % Versions.sbtLucuma)
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"  % Versions.sbtBuildInfo)
addSbtPlugin("edu.gemini"   % "sbt-clue"       % Versions.clue)
