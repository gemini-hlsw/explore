// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.data.NonEmptyMap
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import coulomb._
import coulomb.si.Kelvin
import crystal.react.ReuseView
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.common._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Band
import lucuma.core.enum.CoolStarTemperature
import lucuma.core.enum.GalaxySpectrum
import lucuma.core.enum.HIIRegionSpectrum
import lucuma.core.enum.PlanetSpectrum
import lucuma.core.enum.PlanetaryNebulaSpectrum
import lucuma.core.enum.QuasarSpectrum
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumSelect
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import queries.schemas.implicits._
import react.common.ReactFnProps
import react.semanticui.elements.label.LabelPointing

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

sealed trait SpectralDefinitionEditor[T, S] {
  val spectralDefinition: ReuseAligner[SpectralDefinition[T], S]

  val toInput: SpectralDefinition[T] => S

  implicit val appCtx: AppContextIO

  val sedAlignerOpt: Option[ReuseAligner[UnnormalizedSED, UnnormalizedSedInput]]

  val bandBrightnessesViewOpt: Option[ReuseView[SortedMap[Band, BrightnessMeasure[T]]]]

  val emissionLinesViewOpt: Option[ReuseView[SortedMap[Wavelength, EmissionLine[T]]]]

  val fluxDensityContinuumOpt: Option[
    ReuseView[Measure[PosBigDecimal] Of FluxDensityContinuum[T]]
  ]
}

sealed abstract class SpectralDefinitionEditorBuilder[
  T,
  S,
  Props <: SpectralDefinitionEditor[T, S]
](implicit
  enumFDCUnits: Enumerated[Units Of FluxDensityContinuum[T]]
) {
  import SpectralDefinition._
  import UnnormalizedSED._

  protected val brightnessEditor: ReuseView[SortedMap[Band, BrightnessMeasure[T]]] => VdomNode
  protected val emissionLineEditor: ReuseView[SortedMap[Wavelength, EmissionLine[T]]] => VdomNode

  private def toBandNormalized[T](
    sed: UnnormalizedSED
  ): SpectralDefinition[T] => SpectralDefinition[T] =
    _ match {
      case BandNormalized(_, bs) => BandNormalized(sed, bs)
      case EmissionLines(_, _)   => BandNormalized(sed, SortedMap.empty)
    }

  private sealed abstract class SEDType(
    val name:    String,
    val convert: SpectralDefinition[T] => SpectralDefinition[T]
  ) extends Product
      with Serializable

  private sealed abstract class BandNormalizedSED(name: String, sed: UnnormalizedSED)
      extends SEDType(name, toBandNormalized(sed))

  private object SEDType {

    case object StellarLibraryType
        extends BandNormalizedSED("Stellar Library", StellarLibrary(StellarLibrarySpectrum.O5V))
    case object CoolStarModelType
        extends BandNormalizedSED("Cool Star Model", CoolStarModel(CoolStarTemperature.T400K))
    case object GalaxyType   extends BandNormalizedSED("Galaxy", Galaxy(GalaxySpectrum.Spiral))
    case object PlanetType   extends BandNormalizedSED("Planet", Planet(PlanetSpectrum.Mars))
    case object QuasarType   extends BandNormalizedSED("Quasar", Quasar(QuasarSpectrum.QS0))
    case object HIIRegionType
        extends BandNormalizedSED("HII Region", HIIRegion(HIIRegionSpectrum.OrionNebula))
    case object PlanetaryNebulaType
        extends BandNormalizedSED("Planetary Nebula",
                                  PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
        )
    case object EmissionLineType
        extends SEDType(
          "Emission Line",
          _ =>
            EmissionLines[T](
              SortedMap.empty,
              enumFDCUnits.all.head.withValueTagged(PosBigDecimal(BigDecimal(1)))
            )
        )
    case object PowerLawType extends BandNormalizedSED("Power Law", PowerLaw(BigDecimal(0)))
    case object BlackBodyType
        extends BandNormalizedSED("Black Body", BlackBody(PosInt(1000).withUnit[Kelvin]))
    case object UserDefinedType
        extends BandNormalizedSED("User Defined",
                                  UserDefined(
                                    null.asInstanceOf[NonEmptyMap[Wavelength, PosBigDecimal]]
                                  )
        )

    implicit val enumSEDType: Enumerated[SEDType] =
      Enumerated
        .from[SEDType](
          StellarLibraryType,
          CoolStarModelType,
          GalaxyType,
          PlanetType,
          QuasarType,
          HIIRegionType,
          PlanetaryNebulaType,
          EmissionLineType,
          PowerLawType,
          BlackBodyType,
          UserDefinedType
        )
        .withTag(_.name)
  }

  val component = ScalaFnComponent[Props] { props =>
    import props._

    val stellarLibrarySpectrumAlignerOpt
      : Option[ReuseAligner[StellarLibrarySpectrum, Input[StellarLibrarySpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.stellarLibrary.andThen(
            UnnormalizedSED.StellarLibrary.librarySpectrum
          ),
          UnnormalizedSedInput.stellarLibrary.modify
        )
      )

    val coolStarTemperatureAlignerOpt
      : Option[ReuseAligner[CoolStarTemperature, Input[CoolStarTemperature]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.coolStarModel.andThen(UnnormalizedSED.CoolStarModel.temperature),
          UnnormalizedSedInput.coolStar.modify
        )
      )

    val galaxySpectrumAlignerOpt: Option[ReuseAligner[GalaxySpectrum, Input[GalaxySpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.galaxy.andThen(UnnormalizedSED.Galaxy.galaxySpectrum),
          UnnormalizedSedInput.galaxy.modify
        )
      )

    val planetSpectrumAlignerOpt: Option[ReuseAligner[PlanetSpectrum, Input[PlanetSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planet.andThen(UnnormalizedSED.Planet.planetSpectrum),
          UnnormalizedSedInput.planet.modify
        )
      )

    val quasarSpectrumAlignerOpt: Option[ReuseAligner[QuasarSpectrum, Input[QuasarSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.quasar.andThen(UnnormalizedSED.Quasar.quasarSpectrum),
          UnnormalizedSedInput.quasar.modify
        )
      )

    val hiiRegionSpectrumAlignerOpt
      : Option[ReuseAligner[HIIRegionSpectrum, Input[HIIRegionSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.hiiRegion.andThen(UnnormalizedSED.HIIRegion.hiiRegionSpectrum),
          UnnormalizedSedInput.hiiRegion.modify
        )
      )

    val planetaryNebulaSpectrumAlignerOpt
      : Option[ReuseAligner[PlanetaryNebulaSpectrum, Input[PlanetaryNebulaSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planetaryNebula.andThen(
            UnnormalizedSED.PlanetaryNebula.planetaryNebulaSpectrum
          ),
          UnnormalizedSedInput.planetaryNebula.modify
        )
      )

    val powerLawIndexAlignerOpt: Option[ReuseAligner[BigDecimal, Input[BigDecimal]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.powerLaw.andThen(UnnormalizedSED.PowerLaw.index),
          UnnormalizedSedInput.powerLaw.modify
        )
      )

    val blackBodyTemperatureAlignerOpt
      : Option[ReuseAligner[Quantity[PosInt, Kelvin], Input[PosInt]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.blackBody.andThen(UnnormalizedSED.BlackBody.temperature),
          UnnormalizedSedInput.blackBodyTempK.modify
        )
      )

    val currentType: SEDType =
      props.spectralDefinition.get match {
        case BandNormalized(StellarLibrary(_), _)  => SEDType.StellarLibraryType
        case BandNormalized(CoolStarModel(_), _)   => SEDType.CoolStarModelType
        case BandNormalized(Galaxy(_), _)          => SEDType.GalaxyType
        case BandNormalized(Planet(_), _)          => SEDType.PlanetType
        case BandNormalized(Quasar(_), _)          => SEDType.QuasarType
        case BandNormalized(HIIRegion(_), _)       => SEDType.HIIRegionType
        case BandNormalized(PlanetaryNebula(_), _) => SEDType.PlanetaryNebulaType
        case EmissionLines(_, _)                   => SEDType.EmissionLineType
        case BandNormalized(PowerLaw(_), _)        => SEDType.PowerLawType
        case BandNormalized(BlackBody(_), _)       => SEDType.BlackBodyType
        case BandNormalized(UserDefined(_), _)     => SEDType.UserDefinedType
      }

    def spectrumRow[T: Enumerated](id: string.NonEmptyString, view: ReuseView[T]) =
      React.Fragment(
        <.span,
        EnumViewSelect(id, view.value),
        <.span
      )

    React.Fragment(
      <.label("SED", ExploreStyles.SkipToNext),
      EnumSelect[SEDType](
        label = "",
        value = currentType.some,
        onChange = Reuse.by(props.spectralDefinition)((sed: SEDType) =>
          props.spectralDefinition.view(props.toInput).mod(sed.convert)
        ),
        disabledItems = HashSet(SEDType.UserDefinedType)
      ),
      <.span,
      stellarLibrarySpectrumAlignerOpt
        .map(rsu => spectrumRow("slSpectrum", rsu.map(_.view(_.assign)))),
      coolStarTemperatureAlignerOpt
        .map(rsu => spectrumRow("csTemp", rsu.map(_.view(_.assign)))),
      galaxySpectrumAlignerOpt
        .map(rsu => spectrumRow("gSpectrum", rsu.map(_.view(_.assign)))),
      planetSpectrumAlignerOpt
        .map(rsu => spectrumRow("pSpectrum", rsu.map(_.view(_.assign)))),
      quasarSpectrumAlignerOpt
        .map(rsu => spectrumRow("qSpectrum", rsu.map(_.view(_.assign)))),
      hiiRegionSpectrumAlignerOpt
        .map(rsu => spectrumRow("hiirSpectrum", rsu.map(_.view(_.assign)))),
      planetaryNebulaSpectrumAlignerOpt
        .map(rsu => spectrumRow("pnSpectrum", rsu.map(_.view(_.assign)))),
      powerLawIndexAlignerOpt
        .map(rsu =>
          React.Fragment(
            <.label("Index", ExploreStyles.SkipToNext),
            FormInputEV( // Power-law index can be any decimal
              id = "powerLawIndex",
              value = rsu.view(_.assign),
              validFormat = ValidFormatInput.bigDecimalValidFormat(),
              changeAuditor =
                ChangeAuditor.fromValidFormatInput(ValidFormatInput.bigDecimalValidFormat()),
              errorClazz = ExploreStyles.InputErrorTooltip,
              errorPointing = LabelPointing.Below
            ),
            <.span
          )
        ),
      blackBodyTemperatureAlignerOpt
        .map(rsu =>
          React.Fragment(
            <.label("Temperature", ExploreStyles.SkipToNext),
            InputWithUnits( // Temperature is in K, a positive integer
              rsu.map(_.view(_.value.assign)).stripQuantity,
              ValidFormatInput.forRefinedInt[Positive](),
              ChangeAuditor
                .fromValidFormatInput(ValidFormatInput.forRefinedInt[Positive]())
                .denyNeg,
              id = "bbTempK",
              units = "°K"
            ),
            <.span
          )
        ),
      props.bandBrightnessesViewOpt
        .map(bandBrightnessesView =>
          <.div(ExploreStyles.BrightnessesTableWrapper, brightnessEditor(bandBrightnessesView))
        ),
      props.fluxDensityContinuumOpt
        .map(fluxDensityContinuum =>
          React.Fragment(
            <.label("Continuum", ExploreStyles.SkipToNext),
            FormInputEV[ReuseView, PosBigDecimal](
              id = "fluxValue",
              value = fluxDensityContinuum.zoom(
                Measure.valueTagged[PosBigDecimal, FluxDensityContinuum[T]]
              ),
              validFormat = ValidFormatInput.forScientificNotationPosBigDecimal(),
              changeAuditor = ChangeAuditor.posScientificNotation()
            ),
            EnumViewSelect[ReuseView, Units Of FluxDensityContinuum[T]](
              "Units",
              fluxDensityContinuum
                .zoom(Measure.unitsTagged[PosBigDecimal, FluxDensityContinuum[T]])
            )
          )
        ),
      props.emissionLinesViewOpt.map(e =>
        <.div(ExploreStyles.BrightnessesTableWrapper, emissionLineEditor(e))
      )
    )
  }

}

final case class IntegratedSpectralDefinitionEditor(
  val spectralDefinition: ReuseAligner[SpectralDefinition[Integrated],
                                       SpectralDefinitionIntegratedInput
  ]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[IntegratedSpectralDefinitionEditor](
      IntegratedSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Integrated, SpectralDefinitionIntegratedInput] {
  val toInput: SpectralDefinition[Integrated] => SpectralDefinitionIntegratedInput = _.toInput

  private val bandNormalizedAlignerOpt: Option[
    ReuseAligner[
      SpectralDefinition.BandNormalized[Integrated],
      BandNormalizedIntegratedInput
    ]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.bandNormalized[Integrated],
      forceAssign(SpectralDefinitionIntegratedInput.bandNormalized.modify)(
        BandNormalizedIntegratedInput()
      )
    )

  val sedAlignerOpt: Option[ReuseAligner[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.sed[Integrated],
             forceAssign(BandNormalizedIntegratedInput.sed.modify)(
               UnnormalizedSedInput()
             )
      )
    )

  val bandBrightnessesViewOpt: Option[ReuseView[SortedMap[Band, BrightnessMeasure[Integrated]]]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.brightnesses[Integrated],
             BandNormalizedIntegratedInput.brightnesses.modify
      )
        .view(_.toInput.assign)
    )

  private val emissionLinesAlignerOpt: Option[
    ReuseAligner[SpectralDefinition.EmissionLines[Integrated], EmissionLinesIntegratedInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.emissionLines[Integrated],
      forceAssign(SpectralDefinitionIntegratedInput.emissionLines.modify)(
        EmissionLinesIntegratedInput()
      )
    )

  override val emissionLinesViewOpt
    : Option[ReuseView[SortedMap[Wavelength, EmissionLine[Integrated]]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.lines[Integrated],
             EmissionLinesIntegratedInput.lines.modify
      )
        .view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt
    : Option[ReuseView[Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]]] =
    emissionLinesAlignerOpt.map(
      _.map(
        _.zoom(SpectralDefinition.EmissionLines.fluxDensityContinuum[Integrated],
               EmissionLinesIntegratedInput.fluxDensityContinuum.modify
        )
          .view(_.toInput.assign)
      )
    )
}

object IntegratedSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[Integrated,
                                            SpectralDefinitionIntegratedInput,
                                            IntegratedSpectralDefinitionEditor
    ] {
  protected val brightnessEditor
    : ReuseView[SortedMap[Band, BrightnessMeasure[Integrated]]] => VdomNode =
    brightnessesView => IntegratedBrightnessEditor(brightnessesView, false)

  protected val emissionLineEditor
    : ReuseView[SortedMap[Wavelength, EmissionLine[Integrated]]] => VdomNode =
    emissionLinesView => IntegratedEmissionLineEditor(emissionLinesView, false)
}

final case class SurfaceSpectralDefinitionEditor(
  val spectralDefinition: ReuseAligner[SpectralDefinition[Surface], SpectralDefinitionSurfaceInput]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[SurfaceSpectralDefinitionEditor](
      SurfaceSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Surface, SpectralDefinitionSurfaceInput] {

  val toInput: SpectralDefinition[Surface] => SpectralDefinitionSurfaceInput = _.toInput

  private val bandNormalizedAlignerOpt: Option[
    ReuseAligner[SpectralDefinition.BandNormalized[Surface], BandNormalizedSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.bandNormalized[Surface],
      forceAssign(SpectralDefinitionSurfaceInput.bandNormalized.modify)(
        BandNormalizedSurfaceInput()
      )
    )

  val sedAlignerOpt: Option[ReuseAligner[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedAlignerOpt.map(
      _.map(
        _.zoom(SpectralDefinition.BandNormalized.sed[Surface],
               forceAssign(BandNormalizedSurfaceInput.sed.modify)(
                 UnnormalizedSedInput()
               )
        )
      )
    )

  val bandBrightnessesViewOpt: Option[ReuseView[SortedMap[Band, BrightnessMeasure[Surface]]]] =
    bandNormalizedAlignerOpt.map(
      _.map(
        _.zoom(SpectralDefinition.BandNormalized.brightnesses[Surface],
               BandNormalizedSurfaceInput.brightnesses.modify
        )
          .view(_.toInput.assign)
      )
    )

  private val emissionLinesAlignerOpt: Option[
    ReuseAligner[SpectralDefinition.EmissionLines[Surface], EmissionLinesSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(SpectralDefinition.emissionLines[Surface],
                               forceAssign(SpectralDefinitionSurfaceInput.emissionLines.modify)(
                                 EmissionLinesSurfaceInput()
                               )
    )

  override val emissionLinesViewOpt
    : Option[ReuseView[SortedMap[Wavelength, EmissionLine[Surface]]]] =
    emissionLinesAlignerOpt.map(
      _.map(
        _.zoom(SpectralDefinition.EmissionLines.lines[Surface],
               EmissionLinesSurfaceInput.lines.modify
        )
          .view(_.toInput.assign)
      )
    )

  override val fluxDensityContinuumOpt
    : Option[ReuseView[Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]]] =
    emissionLinesAlignerOpt.map(
      _.map(
        _.zoom(SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface],
               EmissionLinesSurfaceInput.fluxDensityContinuum.modify
        )
          .view(_.toInput.assign)
      )
    )
}

object SurfaceSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[Surface,
                                            SpectralDefinitionSurfaceInput,
                                            SurfaceSpectralDefinitionEditor
    ] {
  protected val brightnessEditor
    : ReuseView[SortedMap[Band, BrightnessMeasure[Surface]]] => VdomNode =
    brightnessesView => SurfaceBrightnessEditor(brightnessesView, false)

  protected val emissionLineEditor
    : ReuseView[SortedMap[Wavelength, EmissionLine[Surface]]] => VdomNode =
    emissionLinesView => SurfaceEmissionLineEditor(emissionLinesView, false)

}
