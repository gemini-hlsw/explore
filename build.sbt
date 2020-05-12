import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import Settings.Libraries._

val reactJS = "16.7.0"
val SUI     = "2.4.1"

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

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports"
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
    libraryDependencies ++=
      ReactAladin.value
  )

lazy val conditions = project
  .in(file("conditions"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)

lazy val explore: Project = project
  .in(file("explore"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    libraryDependencies ++=
      ReactCommon.value ++
        ReactGridLayout.value ++
        ReactSizeMe.value,
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
  libraryDependencies ++= (Seq("edu.gemini" %%% "gsp-core-model" % "0.1.8") ++
    Cats.value ++
    Mouse.value ++
    CatsEffect.value ++
    Log4Cats.value ++
    Monocle.value ++
    Circe.value ++
    Crystal.value ++
    Clue.value ++
    DiodeData.value ++
    In(Test)(
      MUnit.value ++
        Discipline.value ++
        MonocleLaw.value ++
        GSPMathTestKit.value ++
        GSPCoreTestKit.value
    )),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val commonJsLibSettings = gspScalaJsSettings ++ commonLibSettings ++ Seq(
  libraryDependencies ++=
    ScalaJSReact.value ++
      ReactSemanticUI.value ++
      ClueScalaJS.value ++
      DiodeReact.value ++
      GPPUI.value ++
      In(Test)(
        ScalaJSReactTest.value
      )
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
