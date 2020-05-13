resolvers in Global += Resolver.sonatypeRepo("public")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.17.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.4")

addSbtPlugin("edu.gemini" % "sbt-gsp" % "0.2.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
