// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import coulomb._
import coulomb.si.Kelvin
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.common._
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import explore.model.enums.IntegratedSEDType
import explore.model.enums.IntegratedSEDType._
import explore.model.enums.SEDType
import explore.model.enums.SurfaceSEDType
import explore.model.enums.SurfaceSEDType._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Band
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.InputValidSplitEpi
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumSelect
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import queries.schemas.implicits._
import react.common.ReactFnProps
import react.semanticui.elements.label.LabelPointing

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

sealed trait SpectralDefinitionEditor[T, S] {
  val spectralDefinition: Aligner[SpectralDefinition[T], S]

  val toInput: SpectralDefinition[T] => S

  implicit val appCtx: AppContextIO

  val sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]]

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]

  val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]

  val fluxDensityContinuumOpt: Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[T]]]
}

sealed abstract class SpectralDefinitionEditorBuilder[
  T,
  S,
  Props <: SpectralDefinitionEditor[T, S]
](implicit
  sedTypeEnum:    Enumerated[SEDType[T]],
  sedTypeDisplay: Display[SEDType[T]],
  enumFDCUnits:   Enumerated[Units Of FluxDensityContinuum[T]]
) {
  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[T]]] => VdomNode
  protected val emissionLineEditor: View[SortedMap[Wavelength, EmissionLine[T]]] => VdomNode

  protected val currentType: SpectralDefinition[T] => SEDType[T]
  protected val disabledItems: HashSet[SEDType[T]]

  val component = ScalaFnComponent[Props] { props =>
    import props._

    val stellarLibrarySpectrumAlignerOpt
      : Option[Aligner[StellarLibrarySpectrum, Input[StellarLibrarySpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.stellarLibrary.andThen(
            UnnormalizedSED.StellarLibrary.librarySpectrum
          ),
          UnnormalizedSedInput.stellarLibrary.modify
        )
      )

    val coolStarTemperatureAlignerOpt
      : Option[Aligner[CoolStarTemperature, Input[CoolStarTemperature]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.coolStarModel.andThen(UnnormalizedSED.CoolStarModel.temperature),
          UnnormalizedSedInput.coolStar.modify
        )
      )

    val galaxySpectrumAlignerOpt: Option[Aligner[GalaxySpectrum, Input[GalaxySpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.galaxy.andThen(UnnormalizedSED.Galaxy.galaxySpectrum),
          UnnormalizedSedInput.galaxy.modify
        )
      )

    val planetSpectrumAlignerOpt: Option[Aligner[PlanetSpectrum, Input[PlanetSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planet.andThen(UnnormalizedSED.Planet.planetSpectrum),
          UnnormalizedSedInput.planet.modify
        )
      )

    val quasarSpectrumAlignerOpt: Option[Aligner[QuasarSpectrum, Input[QuasarSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.quasar.andThen(UnnormalizedSED.Quasar.quasarSpectrum),
          UnnormalizedSedInput.quasar.modify
        )
      )

    val hiiRegionSpectrumAlignerOpt: Option[Aligner[HIIRegionSpectrum, Input[HIIRegionSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.hiiRegion.andThen(UnnormalizedSED.HIIRegion.hiiRegionSpectrum),
          UnnormalizedSedInput.hiiRegion.modify
        )
      )

    val planetaryNebulaSpectrumAlignerOpt
      : Option[Aligner[PlanetaryNebulaSpectrum, Input[PlanetaryNebulaSpectrum]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.planetaryNebula.andThen(
            UnnormalizedSED.PlanetaryNebula.planetaryNebulaSpectrum
          ),
          UnnormalizedSedInput.planetaryNebula.modify
        )
      )

    val powerLawIndexAlignerOpt: Option[Aligner[BigDecimal, Input[BigDecimal]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.powerLaw.andThen(UnnormalizedSED.PowerLaw.index),
          UnnormalizedSedInput.powerLaw.modify
        )
      )

    val blackBodyTemperatureAlignerOpt: Option[Aligner[Quantity[PosInt, Kelvin], Input[PosInt]]] =
      props.sedAlignerOpt.flatMap(
        _.zoomOpt(
          UnnormalizedSED.blackBody.andThen(UnnormalizedSED.BlackBody.temperature),
          UnnormalizedSedInput.blackBodyTempK.modify
        )
      )

    def spectrumRow[T: Enumerated: Display](id: string.NonEmptyString, view: View[T]) =
      React.Fragment(
        <.span,
        EnumViewSelect(id, view),
        <.span
      )

    React.Fragment(
      <.label("SED", ExploreStyles.SkipToNext),
      EnumSelect[SEDType[T]](
        label = "",
        value = currentType(props.spectralDefinition.get).some,
        onChange = sed => props.spectralDefinition.view(props.toInput).mod(sed.convert),
        disabledItems = disabledItems
      ),
      <.span,
      stellarLibrarySpectrumAlignerOpt
        .map(rsu => spectrumRow("slSpectrum", rsu.view(_.assign))),
      coolStarTemperatureAlignerOpt
        .map(rsu => spectrumRow("csTemp", rsu.view(_.assign))),
      galaxySpectrumAlignerOpt
        .map(rsu => spectrumRow("gSpectrum", rsu.view(_.assign))),
      planetSpectrumAlignerOpt
        .map(rsu => spectrumRow("pSpectrum", rsu.view(_.assign))),
      quasarSpectrumAlignerOpt
        .map(rsu => spectrumRow("qSpectrum", rsu.view(_.assign))),
      hiiRegionSpectrumAlignerOpt
        .map(rsu => spectrumRow("hiirSpectrum", rsu.view(_.assign))),
      planetaryNebulaSpectrumAlignerOpt
        .map(rsu => spectrumRow("pnSpectrum", rsu.view(_.assign))),
      powerLawIndexAlignerOpt
        .map(rsu =>
          React.Fragment(
            <.label("Index", ExploreStyles.SkipToNext),
            FormInputEV( // Power-law index can be any decimal
              id = "powerLawIndex",
              value = rsu.view(_.assign),
              validFormat = InputValidSplitEpi.bigDecimal,
              changeAuditor = ChangeAuditor.fromInputValidSplitEpi(InputValidSplitEpi.bigDecimal),
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
              id = "bbTempK",
              value = rsu.view(_.value.assign).stripQuantity,
              validFormat = InputValidSplitEpi.posInt,
              changeAuditor = ChangeAuditor
                .fromInputValidSplitEpi(InputValidSplitEpi.posInt)
                .denyNeg,
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
            FormInputEV(
              id = "fluxValue",
              value = fluxDensityContinuum.zoom(
                Measure.valueTagged[PosBigDecimal, FluxDensityContinuum[T]]
              ),
              validFormat = InputValidSplitEpi.posBigDecimalWithScientificNotation,
              changeAuditor = ChangeAuditor.posScientificNotation()
            ),
            EnumViewSelect(
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
  val spectralDefinition: Aligner[SpectralDefinition[Integrated], SpectralDefinitionIntegratedInput]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[IntegratedSpectralDefinitionEditor](
      IntegratedSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Integrated, SpectralDefinitionIntegratedInput] {
  val toInput: SpectralDefinition[Integrated] => SpectralDefinitionIntegratedInput = _.toInput

  private val bandNormalizedAlignerOpt: Option[
    Aligner[
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

  val sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.sed[Integrated],
             forceAssign(BandNormalizedIntegratedInput.sed.modify)(
               UnnormalizedSedInput()
             )
      )
    )

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[Integrated]]]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(SpectralDefinition.BandNormalized.brightnesses[Integrated],
             BandNormalizedIntegratedInput.brightnesses.modify
      )
        .view(_.toInput.assign)
    )

  private val emissionLinesAlignerOpt: Option[
    Aligner[SpectralDefinition.EmissionLines[Integrated], EmissionLinesIntegratedInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.emissionLines[Integrated],
      forceAssign(SpectralDefinitionIntegratedInput.emissionLines.modify)(
        EmissionLinesIntegratedInput()
      )
    )

  override val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[Integrated]]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.lines[Integrated],
             EmissionLinesIntegratedInput.lines.modify
      )
        .view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt
    : Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(
        SpectralDefinition.EmissionLines.fluxDensityContinuum[Integrated],
        EmissionLinesIntegratedInput.fluxDensityContinuum.modify
      )
        .view(_.toInput.assign)
    )
}

object IntegratedSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[
      Integrated,
      SpectralDefinitionIntegratedInput,
      IntegratedSpectralDefinitionEditor
    ] {
  override protected val currentType: SpectralDefinition[Integrated] => SEDType[Integrated] =
    IntegratedSEDType.fromSpectralDefinition

  override protected val disabledItems: HashSet[SEDType[Integrated]] =
    HashSet(IntegratedSEDType.UserDefinedType)

  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[Integrated]]] => VdomNode =
    brightnessesView => IntegratedBrightnessEditor(brightnessesView, false)

  protected val emissionLineEditor
    : View[SortedMap[Wavelength, EmissionLine[Integrated]]] => VdomNode =
    emissionLinesView => IntegratedEmissionLineEditor(emissionLinesView, false)
}

final case class SurfaceSpectralDefinitionEditor(
  val spectralDefinition: Aligner[SpectralDefinition[Surface], SpectralDefinitionSurfaceInput]
)(implicit val appCtx:    AppContextIO)
    extends ReactFnProps[SurfaceSpectralDefinitionEditor](
      SurfaceSpectralDefinitionEditor.component
    )
    with SpectralDefinitionEditor[Surface, SpectralDefinitionSurfaceInput] {

  val toInput: SpectralDefinition[Surface] => SpectralDefinitionSurfaceInput = _.toInput

  private val bandNormalizedAlignerOpt: Option[
    Aligner[SpectralDefinition.BandNormalized[Surface], BandNormalizedSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.bandNormalized[Surface],
      forceAssign(SpectralDefinitionSurfaceInput.bandNormalized.modify)(
        BandNormalizedSurfaceInput()
      )
    )

  val sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(
        SpectralDefinition.BandNormalized.sed[Surface],
        forceAssign(BandNormalizedSurfaceInput.sed.modify)(UnnormalizedSedInput())
      )
    )

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[Surface]]]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(
        SpectralDefinition.BandNormalized.brightnesses[Surface],
        BandNormalizedSurfaceInput.brightnesses.modify
      )
        .view(_.toInput.assign)
    )

  private val emissionLinesAlignerOpt: Option[
    Aligner[SpectralDefinition.EmissionLines[Surface], EmissionLinesSurfaceInput]
  ] =
    spectralDefinition.zoomOpt(
      SpectralDefinition.emissionLines[Surface],
      forceAssign(SpectralDefinitionSurfaceInput.emissionLines.modify)(
        EmissionLinesSurfaceInput()
      )
    )

  override val emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[Surface]]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(
        SpectralDefinition.EmissionLines.lines[Surface],
        EmissionLinesSurfaceInput.lines.modify
      ).view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt
    : Option[View[Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface],
             EmissionLinesSurfaceInput.fluxDensityContinuum.modify
      ).view(_.toInput.assign)
    )
}

object SurfaceSpectralDefinitionEditor
    extends SpectralDefinitionEditorBuilder[
      Surface,
      SpectralDefinitionSurfaceInput,
      SurfaceSpectralDefinitionEditor
    ] {
  override protected val currentType: SpectralDefinition[Surface] => SEDType[Surface] =
    SurfaceSEDType.fromSpectralDefinition

  override protected val disabledItems: HashSet[SEDType[Surface]] =
    HashSet(SurfaceSEDType.UserDefinedType)

  protected val brightnessEditor: View[SortedMap[Band, BrightnessMeasure[Surface]]] => VdomNode =
    brightnessesView => SurfaceBrightnessEditor(brightnessesView, false)

  protected val emissionLineEditor: View[SortedMap[Wavelength, EmissionLine[Surface]]] => VdomNode =
    emissionLinesView => SurfaceEmissionLineEditor(emissionLinesView, false)

}
