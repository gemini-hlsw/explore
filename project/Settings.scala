import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.librarymanagement._

object Settings {

  object LibraryVersions {
    val cats              = "2.1.1"
    val catsEffect        = "2.1.4"
    val circe             = "0.13.0"
    val clue              = "0.1.1"
    val crystal           = "0.7.1"
    val discipline        = "1.0.3"
    val disciplineMUnit   = "0.2.3"
    val geminiLocales     = "0.5.0"
    val gspCore           = "0.2.8"
    val gspMath           = "0.2.7"
    val gppUI             = "0.2.8"
    val log4Cats          = "1.1.1"
    val log4CatsLogLevel  = "0.0.2"
    val monocle           = "2.0.5"
    val mouse             = "0.25"
    val mUnit             = "0.7.10"
    val reactAladin       = "0.1.12"
    val reactAtlasKitTree = "0.2.3"
    val reactCommon       = "0.9.4"
    val reactGridLayout   = "0.7.0"
    val reactHighcharts   = "0.1.2"
    val reactResizable    = "0.1.2"
    val reactSemanticUI   = "0.6.0"
    val reactSizeMe       = "0.4.4"
    val scalaJsReact      = "1.7.3"
  }

  object Libraries {
    import LibraryVersions._

    private def deps(modules: PlatformDepsGroupArtifactID*)(version: String): Seq[ModuleID]        =
      modules.map(_ % version)

    def In(configuration:     Configuration)(dependencies:           Seq[ModuleID]): Seq[ModuleID] =
      dependencies.map(_ % configuration)

    val Cats = Def.setting(
      deps(
        "org.typelevel" %%% "cats-core"
      )(cats)
    )

    val CatsEffect = Def.setting(
      deps(
        "org.typelevel" %%% "cats-effect"
      )(catsEffect)
    )

    val Circe = Def.setting(
      deps(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser",
        "io.circe" %%% "circe-generic-extras"
      )(circe)
    )

    val Clue = Def.setting(
      deps(
        "com.rpiaggio" %%% "clue-core"
      )(clue)
    )

    val ClueScalaJS = Def.setting(
      deps(
        "com.rpiaggio" %%% "clue-scalajs"
      )(clue)
    )

    val Crystal = Def.setting(
      deps(
        "com.rpiaggio" %%% "crystal"
      )(crystal)
    )

    val Discipline = Def.setting(
      Seq(
        "org.typelevel" %%% "discipline-core"  % discipline,
        "org.typelevel" %%% "discipline-munit" % disciplineMUnit
      )
    )

    val GeminiLocales = Def.setting(
      deps(
        "edu.gemini" %%% "gemini-locales"
      )(geminiLocales)
    )

    val GSPCore = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-core-model"
      )(gspCore)
    )

    val GSPCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-core-testkit"
      )(gspCore)
    )

    val GSPMath = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-math"
      )(gspMath)
    )

    val GSPMathTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-math-testkit"
      )(gspMath)
    )

    val GPPUI = Def.setting(
      deps(
        "edu.gemini" %%% "gpp-ui"
      )(gppUI)
    )

    val Log4Cats = Def.setting(
      Seq(
        "io.chrisdavenport" %%% "log4cats-core"     % log4Cats,
        "com.rpiaggio"      %%% "log4cats-loglevel" % log4CatsLogLevel
      )
    )

    val Monocle = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-core",
        "com.github.julien-truffaut" %%% "monocle-macro"
      )(monocle)
    )

    val MonocleLaw = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-law"
      )(monocle)
    )

    val Mouse = Def.setting(
      deps(
        "org.typelevel" %%% "mouse"
      )(mouse)
    )

    val MUnit = Def.setting(
      deps(
        "org.scalameta" %%% "munit",
        "org.scalameta" %%% "munit-scalacheck"
      )(mUnit)
    )

    val ReactAladin = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-aladin"
      )(reactAladin)
    )

    val ReactAtlasKitTree = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-atlaskit-tree"
      )(
        reactAtlasKitTree
      )
    )

    val ReactCommon = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "common",
        "io.github.cquiroz.react" %%% "cats"
      )(reactCommon)
    )

    val ReactGridLayout = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-grid-layout"
      )(reactGridLayout)
    )

    val ReactHighcharts = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-highcharts"
      )(reactHighcharts)
    )

    val ReactResizable = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-resizable"
      )(reactResizable)
    )

    val ReactSizeMe = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-sizeme"
      )(reactSizeMe)
    )

    val ReactSemanticUI = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-semantic-ui"
      )(reactSemanticUI)
    )

    val ScalaJSReact = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "core",
        "com.github.japgolly.scalajs-react" %%% "extra",
        "com.github.japgolly.scalajs-react" %%% "ext-monocle-cats",
        "com.github.japgolly.scalajs-react" %%% "ext-cats",
        "com.github.japgolly.scalajs-react" %%% "ext-cats-effect"
      )(scalaJsReact)
    )

    val ScalaJSReactTest = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "test"
      )(scalaJsReact)
    )
  }
}
