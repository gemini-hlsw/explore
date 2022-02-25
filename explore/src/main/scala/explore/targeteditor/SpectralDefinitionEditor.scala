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
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.optics._
import explore.schemas.implicits._
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
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.Units._
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
import react.common.ReactFnProps
import react.semanticui.elements.label.LabelPointing

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap
import lucuma.core.model.EmissionLine

sealed trait SpectralDefinitionEditor[T, S] {
  val spectralDefinition: RemoteSyncUndoable[SpectralDefinition[T], S]

  val toInput: SpectralDefinition[T] => S

  implicit val appCtx: AppContextIO

  val sedRSUOpt: Option[RemoteSyncUndoable[UnnormalizedSED, UnnormalizedSedInput]]

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]

  val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]

  val fluxDensityContinuumOpt: Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[T]]]
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

  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[T]]] => VdomNode

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

    val stellarLibrarySpectrumRSUOpt
      : Option[RemoteSyncUndoable[StellarLibrarySpectrum, Input[StellarLibrarySpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.stellarLibrary.andThen(UnnormalizedSED.StellarLibrary.librarySpectrum),
          UnnormalizedSedInput.stellarLibrary.modify
        )
      )

    val coolStarTemperatureRSUOpt
      : Option[RemoteSyncUndoable[CoolStarTemperature, Input[CoolStarTemperature]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.coolStarModel.andThen(UnnormalizedSED.CoolStarModel.temperature),
          UnnormalizedSedInput.coolStar.modify
        )
      )

    val galaxySpectrumRSUOpt: Option[RemoteSyncUndoable[GalaxySpectrum, Input[GalaxySpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.galaxy.andThen(UnnormalizedSED.Galaxy.galaxySpectrum),
          UnnormalizedSedInput.galaxy.modify
        )
      )

    val planetSpectrumRSUOpt: Option[RemoteSyncUndoable[PlanetSpectrum, Input[PlanetSpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planet.andThen(UnnormalizedSED.Planet.planetSpectrum),
          UnnormalizedSedInput.planet.modify
        )
      )

    val quasarSpectrumRSUOpt: Option[RemoteSyncUndoable[QuasarSpectrum, Input[QuasarSpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.quasar.andThen(UnnormalizedSED.Quasar.quasarSpectrum),
          UnnormalizedSedInput.quasar.modify
        )
      )

    val hiiRegionSpectrumRSUOpt
      : Option[RemoteSyncUndoable[HIIRegionSpectrum, Input[HIIRegionSpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.hiiRegion.andThen(UnnormalizedSED.HIIRegion.hiiRegionSpectrum),
          UnnormalizedSedInput.hiiRegion.modify
        )
      )

    val planetaryNebulaSpectrumRSUOpt
      : Option[RemoteSyncUndoable[PlanetaryNebulaSpectrum, Input[PlanetaryNebulaSpectrum]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planetaryNebula.andThen(
            UnnormalizedSED.PlanetaryNebula.planetaryNebulaSpectrum
          ),
          UnnormalizedSedInput.planetaryNebula.modify
        )
      )

    val powerLawIndexRSUOpt: Option[RemoteSyncUndoable[BigDecimal, Input[BigDecimal]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.powerLaw.andThen(UnnormalizedSED.PowerLaw.index),
          UnnormalizedSedInput.powerLaw.modify
        )
      )

    val blackBodyTemperatureRSUOpt: Option[RemoteSyncUndoable[PosInt, Input[BigDecimal]]] =
      props.sedRSUOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.blackBody
            .andThen(UnnormalizedSED.BlackBody.temperature)
            .andThen(coulombIso[PosInt, Kelvin].asLens),
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

    <.div(
      EnumSelect[SEDType](
        label = "SED",
        value = currentType.some,
        onChange = sed => props.spectralDefinition.view(props.toInput).mod(sed.convert),
        disabledItems = HashSet(SEDType.UserDefinedType)
      ),
      stellarLibrarySpectrumRSUOpt
        .map(rsu => EnumViewSelect("slSpectrum", rsu.view(_.assign)))
        .whenDefined,
      coolStarTemperatureRSUOpt
        .map(rsu => EnumViewSelect("csTemp", rsu.view(_.assign)))
        .whenDefined,
      galaxySpectrumRSUOpt
        .map(rsu => EnumViewSelect("gSpectrum", rsu.view(_.assign)))
        .whenDefined,
      planetSpectrumRSUOpt
        .map(rsu => EnumViewSelect("pSpectrum", rsu.view(_.assign)))
        .whenDefined,
      quasarSpectrumRSUOpt
        .map(rsu => EnumViewSelect("qSpectrum", rsu.view(_.assign)))
        .whenDefined,
      hiiRegionSpectrumRSUOpt
        .map(rsu => EnumViewSelect("hiirSpectrum", rsu.view(_.assign)))
        .whenDefined,
      planetaryNebulaSpectrumRSUOpt
        .map(rsu => EnumViewSelect("pnSpectrum", rsu.view(_.assign)))
        .whenDefined,
      powerLawIndexRSUOpt
        .map(rsu =>
          FormInputEV( // Power-law index can be any decimal
            id = "powerLawIndex",
            label = "Index",
            value = rsu.view(_.assign),
            validFormat = ValidFormatInput.bigDecimalValidFormat(),
            changeAuditor =
              ChangeAuditor.fromValidFormatInput(ValidFormatInput.bigDecimalValidFormat()),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below
          )
        )
        .whenDefined,
      blackBodyTemperatureRSUOpt
        .map(rsu =>
          InputWithUnits( // Temperature is in K, a positive integer
            rsu.view(t => BigDecimal(t.value).assign),
            ValidFormatInput.forRefinedInt[Positive](),
            ChangeAuditor
              .fromValidFormatInput(ValidFormatInput.forRefinedInt[Positive]())
              .deny("-"),
            label = "Temperature",
            id = "bbTempK",
            units = "°K"
          )
        )
        .whenDefined,
      props.bandBrightnessesViewOpt
        .map(bandBrightnessesView => brightnessEditor(bandBrightnessesView))
        .whenDefined,
      props.fluxDensityContinuumOpt
        .map(fluxDensityContinuum =>
          React.Fragment(
            FormInputEV( // [View, PosBigDecimal](
              id = "fluxValue",
              value = fluxDensityContinuum.zoom(
                Measure.valueTagged[PosBigDecimal, FluxDensityContinuum[T]]
              ),
              validFormat = ValidFormatInput.forRefinedBigDecimal[Positive](),
              changeAuditor = ChangeAuditor.fromValidFormatInput(
                ValidFormatInput.forRefinedBigDecimal[Positive]()
              )
            ),
            EnumViewSelect(
              "Units",
              fluxDensityContinuum
                .zoom(Measure.unitsTagged[PosBigDecimal, FluxDensityContinuum[T]])
            )
          )
        )
        .whenDefined
    )
  }

}

final case class IntegratedSpectralDefinitionEditor(
  val spectralDefinition: RemoteSyncUndoable[SpectralDefinition[Integrated],
                                             SpectralDefinitionIntegratedInput
  ]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[IntegratedSpectralDefinitionEditor](
      IntegratedSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Integrated, SpectralDefinitionIntegratedInput] {
  val toInput: SpectralDefinition[Integrated] => SpectralDefinitionIntegratedInput = _.toInput

  private val bandNormalizedRSUOpt: Option[
    RemoteSyncUndoable[SpectralDefinition.BandNormalized[Integrated], BandNormalizedIntegratedInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.bandNormalized[Integrated],
      forceAssign(SpectralDefinitionIntegratedInput.bandNormalized.modify)(
        BandNormalizedIntegratedInput()
      )
    )

  val sedRSUOpt: Option[RemoteSyncUndoable[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedRSUOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.sed[Integrated],
             forceAssign(BandNormalizedIntegratedInput.sed.modify)(
               UnnormalizedSedInput()
             )
      )
    )

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[Integrated]]]] =
    bandNormalizedRSUOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.brightnesses[Integrated],
             BandNormalizedIntegratedInput.brightnesses.modify
      )
        .view(_.toInput.assign)
    )

  private val emissionLinesRSUOpt: Option[
    RemoteSyncUndoable[SpectralDefinition.EmissionLines[Integrated], EmissionLinesIntegratedInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.emissionLines[Integrated],
      forceAssign(SpectralDefinitionIntegratedInput.emissionLines.modify)(
        EmissionLinesIntegratedInput()
      )
    )

  override val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[Integrated]]]] =
    emissionLinesRSUOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.lines[Integrated],
             EmissionLinesIntegratedInput.lines.modify
      )
        .view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt
    : Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]]] =
    emissionLinesRSUOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.fluxDensityContinuum[Integrated],
             EmissionLinesIntegratedInput.fluxDensityContinuum.modify
      )
        .view(_.toInput.assign)
    )
}

object IntegratedSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[Integrated,
                                            SpectralDefinitionIntegratedInput,
                                            IntegratedSpectralDefinitionEditor
    ] {
  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[Integrated]]] => VdomNode =
    brightnessesView => IntegratedBrightnessEditor(brightnessesView, false)
}

final case class SurfaceSpectralDefinitionEditor(
  val spectralDefinition: RemoteSyncUndoable[SpectralDefinition[Surface],
                                             SpectralDefinitionSurfaceInput
  ]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[SurfaceSpectralDefinitionEditor](
      SurfaceSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Surface, SpectralDefinitionSurfaceInput] {

  val toInput: SpectralDefinition[Surface] => SpectralDefinitionSurfaceInput = _.toInput

  private val bandNormalizedRSUOpt: Option[
    RemoteSyncUndoable[SpectralDefinition.BandNormalized[Surface], BandNormalizedSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.bandNormalized[Surface],
      forceAssign(SpectralDefinitionSurfaceInput.bandNormalized.modify)(
        BandNormalizedSurfaceInput()
      )
    )

  val sedRSUOpt: Option[RemoteSyncUndoable[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedRSUOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.sed[Surface],
             forceAssign(BandNormalizedSurfaceInput.sed.modify)(
               UnnormalizedSedInput()
             )
      )
    )

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[Surface]]]] =
    bandNormalizedRSUOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.brightnesses[Surface],
             BandNormalizedSurfaceInput.brightnesses.modify
      )
        .view(_.toInput.assign)
    )

  private val emissionLinesRSUOpt: Option[
    RemoteSyncUndoable[SpectralDefinition.EmissionLines[Surface], EmissionLinesSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.emissionLines[Surface],
      forceAssign(SpectralDefinitionSurfaceInput.emissionLines.modify)(
        EmissionLinesSurfaceInput()
      )
    )

  override val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[Surface]]]] =
    emissionLinesRSUOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.lines[Surface],
             EmissionLinesSurfaceInput.lines.modify
      )
        .view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt
    : Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]]] =
    emissionLinesRSUOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface],
             EmissionLinesSurfaceInput.fluxDensityContinuum.modify
      )
        .view(_.toInput.assign)
    )
}

object SurfaceSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[Surface,
                                            SpectralDefinitionSurfaceInput,
                                            SurfaceSpectralDefinitionEditor
    ] {
  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[Surface]]] => VdomNode =
    brightnessesView => SurfaceBrightnessEditor(brightnessesView, false)
}
