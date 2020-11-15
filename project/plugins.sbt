resolvers in Global += Resolver.sonatypeRepo("public")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.3.1")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.20.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.2")
addSbtPlugin("edu.gemini"         % "sbt-lucuma"               % "0.3.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.10.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
