val reactJS      = "16.7.0"
val scalaJsReact = "1.6.0"
val SUI          = "2.4.1"
val circe        = "0.13.0"

parallelExecution in (ThisBuild, Test) := false

cancelable in Global := true

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers in Global += Resolver.sonatypeRepo("public")

addCommandAlias(
  "exploreWDS",
  "; explore/fastOptJS::stopWebpackDevServer; explore/fastOptJS::startWebpackDevServer; ~explore/fastOptJS"
)
addCommandAlias(
  "conditionsWDS",
  "; conditions/fastOptJS::stopWebpackDevServer; conditions/fastOptJS::startWebpackDevServer; ~conditions/fastOptJS"
)

// For Heroku deployment
addCommandAlias(
  "stage",
  "explore/fullOptJS::webpack"
)

lazy val root =
  project
    .in(file("."))
    .settings(name := "explore-root")
    .settings(commonSettings: _*)
    .aggregate(common, conditions, explore)

lazy val common = project
  .in(file("common"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val conditions = project
  .in(file("conditions"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)

lazy val explore: Project = project
  .in(file("explore"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz.react" %%% "common" % "0.7.0",
      "io.github.cquiroz.react" %%% "react-grid-layout" % "0.4.0",
      "io.github.cquiroz.react" %%% "react-sizeme" % "0.3.2"
    ),
    // don't publish the demo
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    Keys.`package` := file("")
  )
  .dependsOn(conditions)

lazy val commonSettings = gspScalaJsSettings ++ Seq(
  scalaVersion := "2.13.1",
  description := "Explore",
  homepage := Some(url("https://github.com/geminihlsw/explore")),
  licenses := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scalacOptions += "-Ymacro-annotations"
)

lazy val commonLibSettings = gspScalaJsSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJsReact,
    "com.github.japgolly.scalajs-react" %%% "extra" % scalaJsReact,
    "com.github.japgolly.scalajs-react" %%% "test" % scalaJsReact % Test,
    "org.typelevel" %%% "cats-effect" % "2.1.2",
    "org.typelevel" %%% "cats-core" % "2.1.1",
    "io.chrisdavenport" %%% "log4cats-core" % "1.0.1",
    "io.chrisdavenport" %%% "log4cats-log4s" % "0.4.0-M1",
    "io.github.cquiroz.react" %%% "react-semantic-ui" % "0.4.4",
    "com.github.julien-truffaut" %%% "monocle-core" % "2.0.4",
    "com.github.julien-truffaut" %%% "monocle-macro" % "2.0.4",
    "com.rpiaggio" %%% "crystal" % "0.0.22",
    "com.rpiaggio" %%% "clue-scalajs" % "0.0.5",
    "io.circe" %%% "circe-generic-extras" % "0.13.0",
    "io.suzaku" %%% "diode-data" % "1.1.7",
    "io.suzaku" %%% "diode-react" % "1.1.7.160"
  ) ++ Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
  ).map(_ % circe)
)

lazy val commonWDS = Seq(
  version in webpack := "4.41.2",
  version in startWebpackDevServer := "3.9.0",
  webpackConfigFile in fastOptJS := Some(
    (sourceDirectory in (common, Compile)).value / "webpack" / "dev.webpack.config.js"
  ),
  webpackConfigFile in fullOptJS := Some(
    (sourceDirectory in (common, Compile)).value / "webpack" / "prod.webpack.config.js"
  ),
  webpackMonitoredDirectories += (resourceDirectory in (common, Compile)).value,
  webpackMonitoredDirectories += ((sourceDirectory in (common, Compile)).value / "webpack"),
  webpackResources := ((sourceDirectory in (common, Compile)).value / "webpack") * "*.js",
  includeFilter in webpackMonitoredFiles := "*",
  useYarn := true,
  webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly(),
  webpackBundlingMode in fullOptJS := BundlingMode.Application,
  test := {},
  emitSourceMaps := false,
  // NPM libs for development, mostly to let webpack do its magic
  npmDevDependencies in Compile ++= Seq(
    "postcss-loader" -> "3.0.0",
    "autoprefixer" -> "9.7.1",
    "url-loader" -> "2.2.0",
    "file-loader" -> "4.2.0",
    "css-loader" -> "3.2.0",
    "style-loader" -> "1.0.0",
    "less" -> "2.7.2",
    "less-loader" -> "4.1.0",
    "webpack-merge" -> "4.2.2",
    "mini-css-extract-plugin" -> "0.8.0",
    "webpack-dev-server-status-bar" -> "1.1.0",
    "cssnano" -> "4.1.10",
    "uglifyjs-webpack-plugin" -> "2.2.0",
    "html-webpack-plugin" -> "3.2.0",
    "optimize-css-assets-webpack-plugin" -> "5.0.3",
    "favicons-webpack-plugin" -> "0.0.9",
    "why-did-you-update" -> "1.0.6",
    "@packtracker/webpack-plugin" -> "2.2.0"
  ),
  npmDependencies in Compile ++= Seq(
    "react" -> reactJS,
    "react-dom" -> reactJS,
    "semantic-ui-less" -> SUI,
    "prop-types" -> "15.7.2"
  )
)
