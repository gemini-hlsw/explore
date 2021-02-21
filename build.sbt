import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import Settings.Libraries._

val reactJS                   = "16.13.1"
val FUILess                   = "2.8.7"
lazy val kindProjectorVersion = "0.11.2"

parallelExecution in (ThisBuild, Test) := false

cancelable in Global := true

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / useLog4J := true

addCommandAlias(
  "exploreWDS",
  "; stopWDS; explore/fastOptJS::startWebpackDevServer; ~explore/fastOptJS"
)

addCommandAlias(
  "constraintsWDS",
  "; constraints/fastOptJS::stopWebpackDevServer; constraints/fastOptJS::startWebpackDevServer; ~constraints/fastOptJS"
)

addCommandAlias(
  "targeteditorWDS",
  "; stopWDS; targeteditor/fastOptJS::startWebpackDevServer; ~targeteditor/fastOptJS"
)

addCommandAlias(
  "observationtreeWDS",
  "; observationtree/fastOptJS::stopWebpackDevServer; observationtree/fastOptJS::startWebpackDevServer; ~observationtree/fastOptJS"
)

addCommandAlias(
  "proposalWDS",
  "; proposal/fastOptJS::stopWebpackDevServer; proposal/fastOptJS::startWebpackDevServer; ~proposal/fastOptJS"
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
  "fixImports",
  "; scalafix OrganizeImports"
)

val stage = taskKey[Unit]("Prepare static files to deploy to Heroku")

inThisBuild(
  Seq(
    addCompilerPlugin(
      ("org.typelevel" % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    )
  )
)

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
  .aggregate(model.jvm,
             model.js,
             common,
             constraints,
             targeteditor,
             observationtree,
             proposal,
             explore
  )

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

lazy val targeteditor = project
  .in(file("targeteditor"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)
  .settings(
    libraryDependencies ++=
      GeminiLocales.value ++
        LucumaCatalog.value ++
        ReactAladin.value ++
        ReactDatepicker.value ++
        ReactHighcharts.value
  )

lazy val constraints = project
  .in(file("constraints"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)

lazy val observationtree = project
  .in(file("observationtree"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonWDS: _*)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(common)
  .settings(
    libraryDependencies ++=
      ReactAtlasKitTree.value,
    npmDependencies in Compile ++= Seq(
      "core-js" -> "2.6.11" // Without this, core-js 3 is used, which conflicts with @babel/runtime-corejs2
    ),
    scalaJSLinkerConfig in (Compile, fastOptJS) ~= { _.withSourceMap(false) },
    scalaJSLinkerConfig in (Compile, fullOptJS) ~= { _.withSourceMap(false) }
  )

lazy val proposal = project
  .in(file("proposal"))
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
        ReactHighcharts.value ++
        ReactResizable.value
  )
  .dependsOn(constraints, targeteditor, observationtree, proposal)

// ***** START: Move to plugin *****
val depsResourceDirs = taskKey[Seq[File]](
  "List of all resource directories from this project and dependencies, both managed and unmanaged."
)
def depsResourceDirsTask(conf: ConfigKey): sbt.Def.Initialize[sbt.Task[Seq[java.io.File]]] =
  Def.taskDyn {
    val thisProjectRef0 = thisProjectRef.value
    Def.task {
      resourceDirectories
        .in(conf)
        .all(ScopeFilter(inDependencies(thisProjectRef0)))
        .value
        .flatten
    }
  }

val clueGenEq =
  settingKey[Boolean]("Include cats.Eq instances for classes generated by clue macros.").withRank(
    KeyRanks.Invisible
  )
Global / clueGenEq := false

val clueGenShow =
  settingKey[Boolean]("Include cats.Show instances for classes generated by clue macros.").withRank(
    KeyRanks.Invisible
  )
Global / clueGenShow := false

val clueGenLenses =
  settingKey[Boolean]("Include monocle.Lens instances for classes generated by clue macros.")
    .withRank(KeyRanks.Invisible)
Global / clueGenLenses := false

val clueGenReusability =
  settingKey[Boolean](
    "Include japgolly.scalajs.react.Reusability instances for classes generated by clue macros."
  ).withRank(KeyRanks.Invisible)
Global / clueGenReusability := false

val clueGeneralSettings = taskKey[Seq[String]]("General macro settings for clue.")
def clueGeneralSettingsTask(conf: ConfigKey): sbt.Def.Initialize[sbt.Task[Seq[String]]] =
  Def.task {
    List(
      s"clue.cats.eq=${clueGenEq.in(conf).value}",
      s"clue.cats.show=${clueGenShow.in(conf).value}",
      s"clue.monocle.lenses=${clueGenLenses.in(conf).value}",
      s"clue.scalajs-react.reusability=${clueGenReusability.in(conf).value}"
    )
  }

val clueSchemaDirSettings = taskKey[Seq[String]]("Schema dirs macro settings for clue.")
def clueSchemaDirSettingsTask(conf: ConfigKey): sbt.Def.Initialize[sbt.Task[Seq[String]]] =
  Def.taskDyn {
    val resourceDirs = depsResourceDirs.in(conf).value
    Def.task {
      resourceDirs.map(f => s"clue.schemaDir=${f.getAbsolutePath}/graphql/schemas")
    }
  }
// ***** END: Move to plugin *****

lazy val commonSettings = Seq(
  scalaVersion := "2.13.4",
  description := "Explore",
  homepage := Some(url("https://github.com/geminihlsw/explore")),
  licenses := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scalacOptions += "-Ymacro-annotations",
  scalacOptions ~= (_.filterNot(
    Set(
      // By necessity facades will have unused params
      "-Wdead-code",
      "-Wunused:params"
    )
  )),
  addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.11.3").cross(CrossVersion.full)),
  // don't publish anything
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  Keys.`package` := file(""),
  // ***** START: Move to plugin *****
  depsResourceDirs in Compile := depsResourceDirsTask(Compile).value,
  Test / depsResourceDirs := depsResourceDirsTask(Test).value,
  clueGeneralSettings := clueGeneralSettingsTask(Compile).value,
  Test / clueGeneralSettings := clueGeneralSettingsTask(Test).value,
  clueSchemaDirSettings := clueSchemaDirSettingsTask(Compile).value,
  Test / clueSchemaDirSettings := clueSchemaDirSettingsTask(Test).value,
  scalacOptions += "-Xmacro-settings:" +
    (clueGeneralSettings.value ++ clueSchemaDirSettings.value).mkString(","),
  Test / scalacOptions += "-Xmacro-settings:" +
    ((Test / clueGeneralSettings).value ++ (Test / clueSchemaDirSettings).value).mkString(","),
  // ***** END: Move to plugin *****
  clueGenEq := true,
  clueGenShow := true,
  clueGenLenses := true,
  clueGenReusability := true
)

lazy val commonLibSettings = Seq(
  libraryDependencies ++=
    LucumaCore.value ++
      Cats.value ++
      Mouse.value ++
      CatsEffect.value ++
      Monocle.value ++
      Circe.value ++
      Crystal.value ++
      Sttp.value ++
      Clue.value ++
      List("edu.gemini" %% "clue-macro" % Settings.LibraryVersions.clue) ++
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
      Chimney.value ++
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
    "react-resize-detector" -> "6.5.0"
  )
)
