import sbtcrossproject.crossProject
import sbtcrossproject.CrossType

val reactJS      = "16.7.0"
val scalaJsReact = "1.7.0"
val SUI          = "2.4.1"
val circe        = "0.13.0"
val monocle      = "2.0.4"
val mUnit        = "0.7.5"

parallelExecution in (ThisBuild, Test) := false

cancelable in Global := true

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias(
  "exploreWDS",
  "; explore/fastOptJS::stopWebpackDevServer; explore/fastOptJS::startWebpackDevServer; ~explore/fastOptJS"
)
addCommandAlias(
  "conditionsWDS",
  "; conditions/fastOptJS::stopWebpackDevServer; conditions/fastOptJS::startWebpackDevServer; ~conditions/fastOptJS"
)
addCommandAlias(
  "stopWDS",
  "fastOptJS::stopWebpackDevServer"
)
addCommandAlias(
  "quickTest",
  "modelJVM/test"
)
addCommandAlias(
  "targeteditorWDS",
  "; targeteditor/fastOptJS::stopWebpackDevServer; targeteditor/fastOptJS::startWebpackDevServer; ~targeteditor/fastOptJS"
)

// For Heroku deployment
val stage = taskKey[Unit]("Stage and clean task")

stage := {
  (explore / Compile / fullOptJS / webpack).value
  // https://devcenter.heroku.com/articles/reducing-the-slug-size-of-play-2-x-applications#using-sbt-to-clean-build-artifacts
  // If needed, caches can be purged manually: https://thoughtbot.com/blog/how-to-reduce-a-large-heroku-compiled-slug-size
  if (sys.env.getOrElse("POST_STAGE_CLEAN", "false").equals("true")) {
    println("Cleaning up...")
    // Remove sbt-scalajs-bundler directory, which includes node_modules.
    val bundlerDir       =
      (explore / Compile / fullOptJS / artifactPath).value.getParentFile.getParentFile
    sbt.IO.delete(bundlerDir)
    // Remove coursier cache
    val coursierCacheDir = csrCacheDirectory.value
    sbt.IO.delete(coursierCacheDir)
  }
}

lazy val root = project
  .in(file("."))
  .settings(name := "explore-root")
  .settings(commonSettings: _*)
  .aggregate(model.jvm, model.js, common, conditions, targeteditor, explore)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("model"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .jsSettings(
    scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val common = project
  .in(file("common"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(model.js)

lazy val targeteditor = project
  .in(file("targeteditor"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)
  .settings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz.react" %%% "react-aladin" % "0.0.6"
    )
  )

lazy val conditions   = project
  .in(file("conditions"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)

lazy val explore: Project    = project
  .in(file("explore"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz.react" %%% "common"            % "0.7.1",
      "io.github.cquiroz.react" %%% "react-grid-layout" % "0.4.0",
      "io.github.cquiroz.react" %%% "react-sizeme"      % "0.3.3"
    ),
    // don't publish the demo
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    Keys.`package` := file("")
  )
  .dependsOn(conditions, targeteditor)

lazy val commonSettings      = Seq(
  scalaVersion := "2.13.1",
  description := "Explore",
  homepage := Some(url("https://github.com/geminihlsw/explore")),
  licenses := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scalacOptions += "-Ymacro-annotations"
)

lazy val commonLibSettings   = Seq(
  libraryDependencies ++= Seq(
    "edu.gemini"                 %%% "gsp-core-model"       % "0.1.8",
    "org.typelevel"              %%% "cats-effect"          % "2.1.3",
    "org.typelevel"              %%% "cats-core"            % "2.1.1",
    "org.typelevel"              %%% "mouse"                % "0.24",
    "io.chrisdavenport"          %%% "log4cats-core"        % "1.0.1",
    "io.chrisdavenport"          %%% "log4cats-log4s"       % "0.4.0-M1",
    "com.github.julien-truffaut" %%% "monocle-core"         % monocle,
    "com.github.julien-truffaut" %%% "monocle-macro"        % monocle,
    "com.rpiaggio"               %%% "crystal"              % "0.2.0",
    "io.circe"                   %%% "circe-generic-extras" % "0.13.0",
    "io.suzaku"                  %%% "diode-data"           % "1.1.7",
    "com.rpiaggio"               %%% "clue-core"            % "0.0.7",
    "edu.gemini"                 %%% "gsp-math-testkit"     % "0.1.17" % Test,
    "edu.gemini"                 %%% "gsp-core-testkit"     % "0.1.8"  % Test
  ) ++ Seq(
    "io.circe" %%% "circe-core",
    "io.circe" %%% "circe-generic",
    "io.circe" %%% "circe-parser"
  ).map(_                          % circe) ++
    Seq(
      "org.scalameta"              %%% "munit"            % mUnit,
      "org.scalameta"              %%% "munit-scalacheck" % mUnit,
      "org.typelevel"              %%% "discipline-core"  % "1.0.2",
      "com.github.julien-truffaut" %%% "monocle-law"      % monocle
    ).map(_ % Test),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val commonJsLibSettings = gspScalaJsSettings ++ commonLibSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.github.japgolly.scalajs-react" %%% "core"              % scalaJsReact,
    "com.github.japgolly.scalajs-react" %%% "extra"             % scalaJsReact,
    "com.github.japgolly.scalajs-react" %%% "test"              % scalaJsReact % Test,
    "io.suzaku"                         %%% "diode-react"       % "1.1.7.160",
    "io.github.cquiroz.react"           %%% "react-semantic-ui" % "0.4.12",
    "com.rpiaggio"                      %%% "clue-scalajs"      % "0.0.7",
    "edu.gemini"                        %%% "gpp-ui"            % "0.0.3"
  )
)

lazy val commonWDS           = Seq(
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
    "postcss-loader"                     -> "3.0.0",
    "autoprefixer"                       -> "9.7.1",
    "url-loader"                         -> "2.2.0",
    "file-loader"                        -> "4.2.0",
    "css-loader"                         -> "3.2.0",
    "style-loader"                       -> "1.0.0",
    "less"                               -> "2.7.2",
    "less-loader"                        -> "4.1.0",
    "webpack-merge"                      -> "4.2.2",
    "mini-css-extract-plugin"            -> "0.8.0",
    "webpack-dev-server-status-bar"      -> "1.1.0",
    "cssnano"                            -> "4.1.10",
    "uglifyjs-webpack-plugin"            -> "2.2.0",
    "html-webpack-plugin"                -> "3.2.0",
    "optimize-css-assets-webpack-plugin" -> "5.0.3",
    "favicons-webpack-plugin"            -> "0.0.9",
    "why-did-you-update"                 -> "1.0.6",
    "@packtracker/webpack-plugin"        -> "2.2.0"
  ),
  npmDependencies in Compile ++= Seq(
    "react"            -> reactJS,
    "react-dom"        -> reactJS,
    "semantic-ui-less" -> SUI,
    "prop-types"       -> "15.7.2"
  )
)
