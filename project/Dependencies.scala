import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.librarymanagement.*
import Versions.*

object Dependencies {

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

  val CoulombRefined = Def.setting(
    deps(
      "com.manyangled" %%% "coulomb-refined"
    )(coulomb)
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

  val CatsEffectTestkit = Def.setting(
    Seq(
      "org.typelevel" %%% "cats-effect-testkit" % catsEffect
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
    )(lucumaCore)
  )

  val LucumaCatalog = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-catalog"
    )(lucumaCore)
  )

  val LucumaCatalogTestKit = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-catalog-testkit"
    )(lucumaCore)
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
      "edu.gemini" %%% "lucuma-react-circular-progressbar",
      "edu.gemini" %%% "lucuma-react-datepicker",
      "edu.gemini" %%% "lucuma-react-draggable",
      "edu.gemini" %%% "lucuma-react-font-awesome",
      "edu.gemini" %%% "lucuma-react-floatingui",
      "edu.gemini" %%% "lucuma-react-grid-layout",
      "edu.gemini" %%% "lucuma-react-highcharts",
      "edu.gemini" %%% "lucuma-react-hotkeys-hooks",
      "edu.gemini" %%% "lucuma-react-markdown",
      "edu.gemini" %%% "lucuma-react-resize-detector",
      "edu.gemini" %%% "lucuma-react-prime-react"
    )(lucumaReact)
  )

  val LucumaRefined = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-refined"
    )(lucumaRefined)
  )

  val LucumaOdbSchema = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-odb-schema"
    )(lucumaOdbSchema)
  )

  val LucumaSchemas = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-schemas",
      "edu.gemini" %%% "lucuma-schemas-model"
    )(lucumaSchemas)
  )

  val LucumaSchemasTestkit = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-schemas-testkit"
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

  val LucumaUITestKit = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-ui-testkit"
    )(lucumaUI)
  )

  val LucumaITCClient = Def.setting(
    deps(
      "edu.gemini" %%% "lucuma-itc-client"
    )(lucumaITC)
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
      "org.scalameta" %%% "munit"
    )(mUnit)
  )

  val MUnitScalaCheck = Def.setting(
    deps(
      "org.scalameta" %%% "munit-scalacheck"
    )(mUnitScalacheck)
  )

  val MUnitCatsEffect = Def.setting(
    deps(
      "org.typelevel" %%% "munit-cats-effect"
    )(mUnitCatsEffect)
  )

  val RefinedAlgebra = Def.setting(
    Seq("edu.gemini" %%% "refined-algebra" % refinedAlgebra)
  )

  val ScalaCollectionContrib = Def.setting(
    deps(
      "org.scala-lang.modules" %%% "scala-collection-contrib"
    )(scalaCollectionContrib)
  )

  val ScalaJSDom = Def.setting(
    deps(
      "org.scala-js" %%% "scalajs-dom"
    )(scalaJsDom)
  )

  val ScalaJsReact = Def.setting(
    deps(
      "com.github.japgolly.scalajs-react" %%% "core-bundle-cb_io",
      "com.github.japgolly.scalajs-react" %%% "extra",
      "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3",
      "com.github.japgolly.scalajs-react" %%% "callback-ext-cats_effect"
    )(scalaJsReact)
  )

  val ScalaJsReactTest = Def.setting(
    deps(
      "com.github.japgolly.scalajs-react" %%% "test"
    )(scalaJsReact)
  )

}
