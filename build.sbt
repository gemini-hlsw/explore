import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import Settings.Libraries._

val reactJS              = "16.13.1"
val FUILess              = "2.8.7"
val kindProjectorVersion = "0.11.3"

addCommandAlias(
  "quickTest",
  "modelTestsJVM/test"
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; scalafmtAll"
)

addCommandAlias(
  "fix",
  "; headerCreateAll; fixImports; scalafmtAll"
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
  val jsFiles = (explore / Compile / fullLinkJS).value
  if (sys.env.getOrElse("POST_STAGE_CLEAN", "false").equals("true")) {
    println("Cleaning up...")
    // Remove coursier cache
    val coursierCacheDir = csrCacheDirectory.value
    sbt.IO.delete(coursierCacheDir)
  }
}

lazy val root = project
  .in(file("."))
  .settings(name := "explore-root")
  .settings(commonSettings: _*)
  .aggregate(model.jvm, model.js, modelTests.jvm, modelTests.js, graphql, common, explore)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("model"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .jsSettings(commonModuleTest: _*)
  .jvmSettings(commonJVMSettings)

lazy val modelTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("model-testkit"))
  .dependsOn(model)
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .settings(testkitLibSettings: _*)
  .jsSettings(commonModuleTest: _*)
  .jvmSettings(commonJVMSettings)

lazy val modelTests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("model-tests"))
  .dependsOn(modelTestkit)
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .jsSettings(commonModuleTest: _*)
  .jvmSettings(commonJVMSettings)

lazy val graphql = project
  .in(file("common-graphql"))
  .dependsOn(model.js)
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .enablePlugins(ScalaJSPlugin)

lazy val common = project
  .in(file("common"))
  .dependsOn(modelTestkit.js)
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonModuleTest: _*)
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
      git.gitHeadCommit,
      "herokuSourceVersion" -> sys.env.get("SOURCE_VERSION"),
      "buildDateTime"       -> System.currentTimeMillis()
    ),
    buildInfoPackage := "explore",
    Compile / sourceGenerators += Def.taskDyn {
      val root    = (ThisBuild / baseDirectory).value.toURI.toString
      val from    = (graphql / Compile / sourceDirectory).value
      val to      = (Compile / sourceManaged).value
      val outFrom = from.toURI.toString.stripSuffix("/").stripPrefix(root)
      val outTo   = to.toURI.toString.stripSuffix("/").stripPrefix(root)
      Def.task {
        (graphql / Compile / scalafix)
          .toTask(s" GraphQLGen --out-from=$outFrom --out-to=$outTo")
          .value
        (to ** "*.scala").get
      }
    }.taskValue
  )
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin)

lazy val explore: Project = project
  .in(file("explore"))
  .dependsOn(model.js, common)
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(esModule: _*)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Test / test := {},
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
      PPrint.value ++
      In(Test)(
        MUnit.value ++
          MUnitCatsEffect.value ++
          Discipline.value ++
          MonocleLaw.value ++
          LucumaCoreTestKit.value
      ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val testkitLibSettings = Seq(
  libraryDependencies ++= Discipline.value ++
    MonocleLaw.value ++
    LucumaCoreTestKit.value
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

lazy val commonModuleTest = Seq(
  Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
)

lazy val esModule = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.SmallestModules
  )),
  Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
    ModuleSplitStyle.FewestModules
  ))
)
