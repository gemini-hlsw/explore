resolvers in Global += Resolver.sonatypeRepo("public")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.6.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.3")
addSbtPlugin("edu.gemini"         % "sbt-lucuma"               % "0.3.8")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.10.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.1")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.29")
addSbtPlugin("com.timushev.sbt"   % "sbt-rewarn"               % "0.1.3")
