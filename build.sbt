import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import Settings.Libraries._

val reactJS                   = "16.13.1"
val FUILess                   = "2.8.7"
lazy val kindProjectorVersion = "0.11.3"

cancelable in Global := true

addCommandAlias(
  "exploreWDS",
  "; stopWDS; explore / Compile / fastOptJS / startWebpackDevServer; ~explore / fastOptJS"
)

addCommandAlias(
  "stopWDS",
  "explore / Compile / fastOptJS / stopWebpackDevServer"
)

addCommandAlias(
  "quickTest",
  "modelJVM/test"
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports"
)

addCommandAlias(
  "graphQLGen",
  "; scalafix GraphQLGen"
)

addCommandAlias(
  "graphQLCheck",
  "; scalafix GraphQLGen --check"
)

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/explore")),
    addCompilerPlugin(
      ("org.typelevel"                    % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    ),
    description := "Explore",
    scalacOptions += "-Ymacro-annotations",
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    scalafixDependencies += "edu.gemini" %% "clue-generator" % Settings.LibraryVersions.clue,
    scalafixScalaBinaryVersion := "2.13"
  ) ++ lucumaPublishSettings
)

val stage = taskKey[Unit]("Prepare static files to deploy to Heroku")

// For simplicity, the build's stage only deals with the explore app.
stage := {
  val jsFiles = (explore / Compile / fullOptJS / webpack).value
  IO.copy(
    List(
      // Copy to stage directory for cases where we build remotely and deploy only static assets to Heroku.
      ((root / baseDirectory).value / "static.json",
       (explore / crossTarget).value / "stage" / "static.json"
      )
    )
  )
  // https://devcenter.heroku.com/articles/reducing-the-slug-size-of-play-2-x-applications#using-sbt-to-clean-build-artifacts
  // If needed, caches can be purged manually: https://thoughtbot.com/blog/how-to-reduce-a-large-heroku-compiled-slug-size
  // UPDATE 2020-10-08: We might not need this since we are not caching in GitHub. Leaving it in case we go back to build in Heroku.
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
  .aggregate(model.jvm, model.js, common, explore)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("model"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .jvmSettings(commonJVMSettings)

lazy val common = project
  .in(file("common"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(
    libraryDependencies ++=
      LucumaSSO.value ++
        LucumaBC.value ++
        LucumaCatalog.value ++
        ReactGridLayout.value ++
        ReactClipboard.value ++
        ReactCommon.value ++
        ReactTable.value,
    buildInfoKeys := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit
    ),
    buildInfoKeys ++= {
      if (sys.env.contains("SBT_IGNORE_BUILDTIME")) Seq(BuildInfoKey.action("buildTime")(0L))
      else Seq(BuildInfoKey.action("buildTime")(System.currentTimeMillis))
    },
    buildInfoPackage := "explore"
  )
  .enablePlugins(ScalaJSBundlerPlugin, BuildInfoPlugin)
  .dependsOn(model.js)

lazy val explore: Project = project
  .in(file("explore"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    libraryDependencies ++=
      GeminiLocales.value ++
        ReactDatepicker.value ++
        ReactAladin.value ++
        ReactAtlasKitTree.value ++
        ReactCommon.value ++
        ReactGridLayout.value ++
        ReactHighcharts.value ++
        ReactResizable.value
  )
  .dependsOn(model.js, common)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  // don't publish anything
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  Keys.`package` := file("")
)

lazy val commonLibSettings = Seq(
  libraryDependencies ++=
    LucumaCore.value ++
      Cats.value ++
      Mouse.value ++
      CatsEffect.value ++
      CatsRetry.value ++
      Monocle.value ++
      Circe.value ++
      Crystal.value ++
      Sttp.value ++
      Clue.value ++
      In(Test)(
        MUnit.value ++
          Discipline.value ++
          MonocleLaw.value ++
          LucumaCoreTestKit.value
      ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val commonJVMSettings = Seq(
  libraryDependencies ++=
    In(Test)(
      CirceGolden.value
    )
)

lazy val commonJsLibSettings = lucumaScalaJsSettings ++ commonLibSettings ++ Seq(
  libraryDependencies ++=
    ScalaJSReact.value ++
      ReactSemanticUI.value ++
      ClueScalaJS.value ++
      LucumaUI.value ++
      Log4Cats.value ++
      In(Test)(
        ScalaJSReactTest.value
      ),
  dependencyOverrides ++= ScalaJSReact.value
)

lazy val commonWDS = Seq(
  webpack / version := "4.44.1",
  startWebpackDevServer / version := "3.11.0",
  fastOptJS / webpackConfigFile := Some(
    (common / Compile / sourceDirectory).value / "webpack" / "dev.webpack.config.js"
  ),
  fullOptJS / webpackConfigFile := Some(
    (common / Compile / sourceDirectory).value / "webpack" / "prod.webpack.config.js"
  ),
  installJsdom / version := "16.4.0",
  webpackMonitoredDirectories += (common / Compile / resourceDirectory).value,
  webpackMonitoredDirectories += ((common / Compile / sourceDirectory).value / "webpack"),
  webpackResources := ((common / Compile / sourceDirectory).value / "webpack") * "*.js",
  webpackMonitoredFiles / includeFilter := "*",
  useYarn := true,
  fastOptJS / webpackBundlingMode := BundlingMode.LibraryOnly(),
  fullOptJS / webpackBundlingMode := BundlingMode.Application,
  test := {},
  Compile / fastOptJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullOptJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  // NPM libs for development, mostly to let webpack do its magic
  Compile / npmDevDependencies ++= Seq(
    "postcss"                       -> "8.1.1",
    "postcss-loader"                -> "4.0.3",
    "autoprefixer"                  -> "10.0.1",
    "url-loader"                    -> "4.1.0",
    "file-loader"                   -> "6.0.0",
    "css-loader"                    -> "3.5.3",
    "style-loader"                  -> "1.2.1",
    // Don't upgrade less until https://github.com/less/less.js/issues/3434 is fixed
    "less"                          -> "3.9.0",
    "less-loader"                   -> "7.0.1",
    "sass"                          -> "1.26.11",
    "sass-loader"                   -> "9.0.2",
    "webpack-merge"                 -> "4.2.2",
    "mini-css-extract-plugin"       -> "0.9.0",
    "webpack-dev-server-status-bar" -> "1.1.2",
    "cssnano"                       -> "4.1.10",
    "terser-webpack-plugin"         -> "4.2.2",
    "html-webpack-plugin"           -> "4.3.0",
    "css-minimizer-webpack-plugin"  -> "1.1.5",
    "favicons-webpack-plugin"       -> "4.2.0",
    "@packtracker/webpack-plugin"   -> "2.3.0"
  ),
  Compile / npmDependencies ++= Seq(
    "react"                 -> reactJS,
    "react-dom"             -> reactJS,
    "react-is"              -> reactJS,
    "fomantic-ui-less"      -> FUILess,
    "prop-types"            -> "15.7.2",
    "react-moon"            -> "2.0.1",
    "styled-components"     -> "5.1.1",
    "react-popper"          -> "2.2.3",
    "ua-parser-js"          -> "0.7.23",
    "react-resize-detector" -> "6.5.0",
    "react-reflex"          -> "4.0.0"
  )
)
