import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.librarymanagement._

object Settings {

  object LibraryVersions {
    val cats              = "2.3.1"
    val catsEffect        = "2.3.1"
    val chimney           = "0.6.1"
    val circe             = "0.13.0"
    val clue              = "0.8.1"
    val crystal           = "0.9.1"
    val discipline        = "1.1.3"
    val disciplineMUnit   = "1.0.4"
    val geminiLocales     = "0.5.1"
    val log4Cats          = "1.1.1"
    val log4CatsLogLevel  = "0.1.1"
    val lucumaCore        = "0.7.5"
    val lucumaCatalog     = "0.3.5"
    val lucumaUI          = "0.11.3"
    val lucumaSSO         = "0.0.9"
    val lucumaBC          = "0.1.0"
    val monocle           = "2.1.0"
    val mouse             = "0.26.2"
    val mUnit             = "0.7.20"
    val reactAladin       = "0.4.2"
    val reactAtlasKitTree = "0.3.0"
    val reactClipboard    = "1.4.3"
    val reactCommon       = "0.11.3"
    val reactDatepicker   = "0.1.0"
    val reactGridLayout   = "0.9.2"
    val reactHighcharts   = "0.2.0"
    val reactResizable    = "0.4.2"
    val reactSemanticUI   = "0.10.3"
    val reactSizeMe       = "0.6.4"
    val scalaJsReact      = "1.7.7"
    val sttp              = "3.0.0-RC15"
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

    val Chimney = Def.setting(
      deps(
        "io.scalaland" %%% "chimney"
      )(chimney)
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
        "edu.gemini" %%% "clue-core"
      )(clue)
    )

    val ClueScalaJS = Def.setting(
      deps(
        "edu.gemini" %%% "clue-scalajs"
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

    val Log4Cats = Def.setting(
      Seq(
        "io.chrisdavenport" %%% "log4cats-core"     % log4Cats,
        "com.rpiaggio"      %%% "log4cats-loglevel" % log4CatsLogLevel
      )
    )

    val LucumaCore = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core"
      )(lucumaCore)
    )

    val LucumaCatalog = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-catalog"
      )(lucumaCatalog)
    )

    val LucumaCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core-testkit"
      )(lucumaCore)
    )

    val LucumaUI = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-ui"
      )(lucumaUI)
    )

    val LucumaSSO = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-sso-frontend-client"
      )(lucumaSSO)
    )

    val LucumaBC = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-broadcast-channel"
      )(lucumaBC)
    )

    val Monocle = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-core",
        "com.github.julien-truffaut" %%% "monocle-state",
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
        "edu.gemini" %%% "react-aladin"
      )(reactAladin)
    )

    val ReactAtlasKitTree = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-atlaskit-tree"
      )(reactAtlasKitTree)
    )

    val ReactClipboard = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-clipboard"
      )(reactClipboard)
    )

    val ReactCommon = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "common",
        "io.github.cquiroz.react" %%% "cats"
      )(reactCommon)
    )

    val ReactDatepicker = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-datepicker"
      )(reactDatepicker)
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

    val Sttp = Def.setting(
      deps(
        "com.softwaremill.sttp.client3" %%% "core",
        "com.softwaremill.sttp.client3" %%% "circe"
      )(sttp)
    )
  }
}
