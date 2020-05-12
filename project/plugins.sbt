resolvers in Global += Resolver.sonatypeRepo("public")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.33")

addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler-sjs06" % "0.17.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.4")

addSbtPlugin("edu.gemini" % "sbt-gsp" % "0.1.17")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
