import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.librarymanagement.*

object Settings {

  object LibraryVersions {
    val boopickle        = "1.4.0"
    val cats             = "2.9.0"
    val catsEffect       = "3.4.2"
    val catsRetry        = "3.1.0"
    val catsTime         = "0.5.1"
    val circe            = "0.14.3"
    val circeGolden      = "0.3.0"
    val clue             = "0.23.1"
    val crystal          = "0.33.8"
    val discipline       = "1.5.1"
    val disciplineMUnit  = "1.0.9"
    val fs2              = "3.4.0"
    val fs2Data          = "1.6.0"
    val fs2Dom           = "0.1.0"
    val geminiLocales    = "0.7.0"
    val http4s           = "0.23.16"
    val http4sDom        = "0.2.3"
    val kittens          = "3.0.0"
    val log4Cats         = "2.5.0"
    val log4CatsLogLevel = "0.3.1"
    val lucumaBC         = "0.4.0"
    val lucumaCore       = "0.60.0"
    val lucumaCatalog    = "0.36.0"
    val lucumaReact      = "0.26.0"
    val lucumaRefined    = "0.1.1"
    val lucumaSchemas    = "0.39.0"
    val lucumaSSO        = "0.4.4"
    val lucumaUI         = "0.61.1"
    val monocle          = "3.1.0"
    val mouse            = "1.2.1"
    val mUnit            = "0.7.29"
    val mUnitCatsEffect  = "1.0.7"
    val pprint           = "0.8.1"
    val reactAladin      = "0.28.0"
    val refinedAlgebra   = "0.1.0"
    val scalaJsDom       = "2.3.0"
    val scalaJsReact     = "2.1.1"
    val webAppUtil       = "2.0.0-RC12"
  }

  object Libraries {
    import LibraryVersions.*

    private def deps(modules: PlatformDepsGroupArtifactID*)(version: String): Seq[ModuleID] =
      modules.map(_ % version)

    def In(configuration: Configuration)(dependencies: Seq[ModuleID]): Seq[ModuleID] =
      dependencies.map(_ % configuration)

      val Boopickle = Def.setting(
        deps(
          "io.suzaku" %%% "boopickle"
        )(boopickle)
      )

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

    val CatsRetry = Def.setting(
      deps(
        "com.github.cb372" %%% "cats-retry"
      )(catsRetry)
    )

    val Circe = Def.setting(
      deps(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      )(circe)
    )

    val CirceGolden = Def.setting(
      Seq(
        "io.circe" %%% "circe-golden" % circeGolden
      )
    )

    val Clue = Def.setting(
      deps(
        "edu.gemini" %%% "clue-core"
      )(clue)
    )

    val ClueGenerator = Def.setting(
      deps(
        "edu.gemini" %%% "clue-generator"
      )(clue)
    )

    val ClueScalaJS = Def.setting(
      deps(
        "edu.gemini" %%% "clue-scalajs"
      )(clue)
    )

    val Crystal = Def.setting(
      deps(
        "edu.gemini" %%% "crystal"
      )(crystal)
    )

    val Discipline = Def.setting(
      Seq(
        "org.typelevel" %%% "discipline-core"  % discipline,
        "org.typelevel" %%% "discipline-munit" % disciplineMUnit
      )
    )

    val CatsTimeTestkit = Def.setting(
      Seq(
        "org.typelevel" %%% "cats-time-testkit" % catsTime
      )
    )

    val FS2 = Def.setting(
      deps(
        "co.fs2" %%% "fs2-core"
      )(fs2)
    )

    val FS2Data = Def.setting(
      deps(
        "org.gnieh" %%% "fs2-data-csv"
      )(fs2Data)
    )

    val FS2IO = Def.setting(
      deps(
        "co.fs2" %%% "fs2-io"
      )(fs2)
    )

    val FS2Node = Def.setting(
      deps(
        "co.fs2" %%% "fs2-node"
      )(fs2)
    )

    val FS2Dom = Def.setting(
      Seq(
        "com.armanbilge" %%% "fs2-dom" % fs2Dom
      )
    )

    val GeminiLocales = Def.setting(
      deps(
        "edu.gemini" %%% "gemini-locales"
      )(geminiLocales)
    )

    val Http4sDom = Def.setting(
      deps(
        "org.http4s" %%% "http4s-dom"
      )(http4sDom)
    )

    val Http4sCore = Def.setting(
      deps(
        "org.http4s" %%% "http4s-core",
        "org.http4s" %%% "http4s-circe",
        "org.http4s" %%% "http4s-client"
      )(http4s)
    )

    val Kittens = Def.setting(
      deps(
        "org.typelevel" %%% "kittens"
      )(kittens)
    )

    val Log4Cats = Def.setting(
      Seq(
        "org.typelevel" %%% "log4cats-core"     % log4Cats,
        "com.rpiaggio"  %%% "log4cats-loglevel" % log4CatsLogLevel
      )
    )

    val LucumaAgs = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-ags"
      )(lucumaCatalog)
    )

    val LucumaBC = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-broadcast-channel"
      )(lucumaBC)
    )

    val LucumaCatalog = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-catalog"
      )(lucumaCatalog)
    )

    val LucumaCatalogTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-catalog-testkit"
      )(lucumaCatalog)
    )

    val LucumaCore = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core"
      )(lucumaCore)
    )

    val LucumaCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core-testkit"
      )(lucumaCore)
    )

    val LucumaReact = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-react-common",
        "edu.gemini" %%% "lucuma-react-tanstack-table",
        "edu.gemini" %%% "lucuma-react-beautiful-dnd",
        "edu.gemini" %%% "lucuma-react-clipboard",
        "edu.gemini" %%% "lucuma-react-circular-progressbar",
        "edu.gemini" %%% "lucuma-react-datepicker",
        "edu.gemini" %%% "lucuma-react-draggable",
        "edu.gemini" %%% "lucuma-react-font-awesome",
        "edu.gemini" %%% "lucuma-react-floatingui",
        "edu.gemini" %%% "lucuma-react-grid-layout",
        "edu.gemini" %%% "lucuma-react-highcharts",
        "edu.gemini" %%% "lucuma-react-hotkeys",
        "edu.gemini" %%% "lucuma-react-resize-detector",
        "edu.gemini" %%% "lucuma-react-semantic-ui",
        "edu.gemini" %%% "lucuma-react-moon",
        "edu.gemini" %%% "lucuma-react-prime-react"
      )(lucumaReact)
    )

    val LucumaRefined = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-refined"
      )(lucumaRefined)
    )

    val LucumaSchemas = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-schemas"
      )(lucumaSchemas)
    )

    val LucumaSSO = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-sso-frontend-client"
      )(lucumaSSO)
    )

    val LucumaUI = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-ui"
      )(lucumaUI)
    )

    val Monocle = Def.setting(
      deps(
        "dev.optics" %%% "monocle-core",
        "dev.optics" %%% "monocle-macro",
        "dev.optics" %%% "monocle-unsafe"
      )(monocle)
    )

    val MonocleLaw = Def.setting(
      deps(
        "dev.optics" %%% "monocle-law"
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

    val MUnitCatsEffect = Def.setting(
      deps(
        "org.typelevel" %%% "munit-cats-effect-3"
      )(mUnitCatsEffect)
    )

    val PPrint = Def.setting(
      deps(
        "com.lihaoyi" %%% "pprint"
      )(pprint)
    )

    val ReactAladin = Def.setting(
      deps(
        "edu.gemini" %%% "react-aladin"
      )(reactAladin)
    )

    val RefinedAlgebra = Def.setting(
      Seq("edu.gemini" %%% "refined-algebra" % refinedAlgebra)
    )

    val ScalaJSDom = Def.setting(
      deps(
        "org.scala-js" %%% "scalajs-dom"
      )(scalaJsDom)
    )

    val ScalaJSReact = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "core-bundle-cb_io",
        "com.github.japgolly.scalajs-react" %%% "extra",
        "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3",
        "com.github.japgolly.scalajs-react" %%% "callback-ext-cats_effect"
      )(scalaJsReact)
    )

    val ScalaJSReactTest = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "test"
      )(scalaJsReact)
    )

    val ScalaWebAppUtil = Def.setting(
      deps(
        "com.github.japgolly.webapp-util" %%% "core",
        "com.github.japgolly.webapp-util" %%% "core-boopickle"
      )(webAppUtil)
    )
  }
}
