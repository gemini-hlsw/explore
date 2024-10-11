// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import coulomb.*
import coulomb.units.si.Kelvin
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.*
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.AppContext
import explore.model.display.given
import explore.model.enums.IntegratedSEDType
import explore.model.enums.IntegratedSEDType.given
import explore.model.enums.SEDType
import explore.model.enums.SurfaceSEDType
import explore.model.enums.SurfaceSEDType.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.FluxDensityContinuumValueRefinement
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.PrimeStyles
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

sealed trait SpectralDefinitionEditor[T, S]:
  def spectralDefinition: Aligner[SpectralDefinition[T], S]
  def catalogInfo: Option[CatalogInfo]
  def brightnessExpanded: View[IsExpanded]
  def disabled: Boolean

  def toInput: SpectralDefinition[T] => S
  def sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]]
  def bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]
  def emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]
  def fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[T]]]

sealed abstract class SpectralDefinitionEditorBuilder[
  T,
  S,
  Props <: SpectralDefinitionEditor[T, S]
](using
  sedTypeEnum:    Enumerated[SEDType[T]],
  sedTypeDisplay: Display[SEDType[T]],
  enumFDCUnits:   Enumerated[Units Of FluxDensityContinuum[T]]
) {
  protected def brightnessEditor
    : (View[SortedMap[Band, BrightnessMeasure[T]]], View[IsExpanded], Boolean) => VdomNode
  protected def emissionLineEditor
    : (View[SortedMap[Wavelength, EmissionLine[T]]], View[IsExpanded], Boolean) => VdomNode

  protected def currentType: SpectralDefinition[T] => Option[SEDType[T]]
  protected def disabledItems: HashSet[SEDType[T]]

  val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .render { (props, ctx) =>
      import ctx.given

      val stellarLibrarySpectrumAlignerOpt
        : Option[Aligner[StellarLibrarySpectrum, Input[StellarLibrarySpectrum]]] =
        props.sedAlignerOpt.flatMap(
          _.zoomOpt(
            UnnormalizedSED.stellarLibrary
              .andThen(
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

      val hiiRegionSpectrumAlignerOpt
        : Option[Aligner[HIIRegionSpectrum, Input[HIIRegionSpectrum]]] =
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
        EnumDropdownView(
          id = id,
          value = view,
          clazz = LucumaPrimeStyles.FormField,
          disabled = props.disabled
        )

      val sed = currentType(props.spectralDefinition.get)

      React.Fragment(
        props.catalogInfo.flatMap(ci =>
          ci.objectType.map(ot =>
            FormInputText(
              id = "catalogInfo".refined,
              value = ot,
              label = React.Fragment(
                ci.catalog match
                  case CatalogName.Import => "Object Type"
                  case other              => other.shortName
                ,
                HelpIcon("target/main/target-catalog-info.md".refined)
              ),
              disabled = true
            )
          )
        ),
        FormLabel(htmlFor = "sed".refined)("SED", HelpIcon("target/main/target-sed.md".refined)),
        <.div(
          ExploreStyles.SEDTypeDropdown |+| ExploreStyles.WarningInput.when_(sed.isEmpty),
          EnumOptionalDropdown[SEDType[T]](
            id = "sed".refined,
            value = sed,
            onChange = sed =>
              props.spectralDefinition
                .view(props.toInput)
                .mod(sed.fold(SpectralDefinition.unnormalizedSED.replace(None))(_.convert)),
            disabledItems = disabledItems,
            clazz = LucumaPrimeStyles.FormField,
            disabled = props.disabled
          ),
          <.span(PrimeStyles.InputGroupAddon, ^.borderRight := 0.px, requiredForITC)
            .when(sed.isEmpty)
        ),
        stellarLibrarySpectrumAlignerOpt
          .map(rsu => spectrumRow("slSpectrum".refined, rsu.view(_.assign))),
        coolStarTemperatureAlignerOpt
          .map(rsu => spectrumRow("csTemp".refined, rsu.view(_.assign))),
        galaxySpectrumAlignerOpt
          .map(rsu => spectrumRow("gSpectrum".refined, rsu.view(_.assign))),
        planetSpectrumAlignerOpt
          .map(rsu => spectrumRow("pSpectrum".refined, rsu.view(_.assign))),
        quasarSpectrumAlignerOpt
          .map(rsu => spectrumRow("qSpectrum".refined, rsu.view(_.assign))),
        hiiRegionSpectrumAlignerOpt
          .map(rsu => spectrumRow("hiirSpectrum".refined, rsu.view(_.assign))),
        planetaryNebulaSpectrumAlignerOpt
          .map(rsu => spectrumRow("pnSpectrum".refined, rsu.view(_.assign))),
        powerLawIndexAlignerOpt
          .map(rsu =>
            FormInputTextView( // Power-law index can be any decimal
              id = "powerLawIndex".refined,
              value = rsu.view(_.assign),
              label = "Index",
              validFormat = InputValidSplitEpi.bigDecimal,
              changeAuditor = ChangeAuditor.fromInputValidSplitEpi(InputValidSplitEpi.bigDecimal),
              disabled = props.disabled
            )
          ),
        blackBodyTemperatureAlignerOpt
          .map(rsu =>
            FormInputTextView( // Temperature is in K, a positive integer
              id = "bbTempK".refined,
              value = rsu.view(_.value.assign).stripQuantity,
              label = "Temperature",
              validFormat = InputValidSplitEpi.posInt,
              changeAuditor = ChangeAuditor
                .fromInputValidSplitEpi(InputValidSplitEpi.posInt)
                .denyNeg,
              units = "°K",
              disabled = props.disabled
            )
          ),
        props.bandBrightnessesViewOpt
          .map(bandBrightnessesView =>
            <.div(ExploreStyles.BrightnessesTableWrapper)(
              brightnessEditor(bandBrightnessesView, props.brightnessExpanded, props.disabled)
            )
          ),
        props.fluxDensityContinuumOpt
          .map(fluxDensityContinuum =>
            React.Fragment(
              FormLabel(htmlFor = "fluxValue".refined)("Continuum"),
              <.div(
                ExploreStyles.FlexContainer |+| LucumaPrimeStyles.FormField,
                FormInputTextView(
                  id = "fluxValue".refined,
                  value = fluxDensityContinuum.zoom(Measure.valueTagged),
                  validFormat = InputValidSplitEpi
                    .refinedBigDecimalWithScientificNotation[FluxDensityContinuumValueRefinement]
                    .andThen(FluxDensityContinuumValue.value.reverse),
                  changeAuditor = ChangeAuditor.posScientificNotation(),
                  disabled = props.disabled
                ),
                EnumDropdownView(
                  id = "Units".refined,
                  value = fluxDensityContinuum.zoom(Measure.unitsTagged),
                  disabled = props.disabled
                )
              )
            )
          ),
        props.emissionLinesViewOpt
          .map(e =>
            <.div(ExploreStyles.BrightnessesTableWrapper)(
              emissionLineEditor(e, props.brightnessExpanded, props.disabled)
            )
          )
      )
    }

}

case class IntegratedSpectralDefinitionEditor(
  spectralDefinition: Aligner[SpectralDefinition[Integrated], SpectralDefinitionIntegratedInput],
  catalogInfo:        Option[CatalogInfo],
  brightnessExpanded: View[IsExpanded],
  disabled:           Boolean
)(using Logger[IO])
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
    bandNormalizedAlignerOpt.flatMap(
      _.zoomOpt(
        SpectralDefinition.BandNormalized.sed[Integrated].some,
        forceAssign(BandNormalizedIntegratedInput.sed.modify)(
          UnnormalizedSedInput()
        )
      )
    )

  val bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[Integrated]]]] =
    bandNormalizedAlignerOpt.map(
      _.zoom(
        SpectralDefinition.BandNormalized.brightnesses[Integrated],
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
      _.zoom(
        SpectralDefinition.EmissionLines.lines[Integrated],
        EmissionLinesIntegratedInput.lines.modify
      )
        .view(_.toInput.assign)
    )

  override val fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[Integrated]]] =
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
  override protected val currentType
    : SpectralDefinition[Integrated] => Option[SEDType[Integrated]] =
    IntegratedSEDType.fromSpectralDefinition

  override protected val disabledItems: HashSet[SEDType[Integrated]] =
    HashSet(IntegratedSEDType.UserDefinedType)

  override protected val brightnessEditor: (
    View[SortedMap[Band, BrightnessMeasure[Integrated]]],
    View[IsExpanded],
    Boolean
  ) => VdomNode =
    (brightnessesView, expanded, disabled) =>
      IntegratedBrightnessEditor(brightnessesView, expanded, disabled)

  override protected val emissionLineEditor: (
    View[SortedMap[Wavelength, EmissionLine[Integrated]]],
    View[IsExpanded],
    Boolean
  ) => VdomNode =
    (emissionLinesView, expanded, disabled) =>
      IntegratedEmissionLineEditor(emissionLinesView, expanded, disabled)
}

case class SurfaceSpectralDefinitionEditor(
  spectralDefinition: Aligner[SpectralDefinition[Surface], SpectralDefinitionSurfaceInput],
  catalogInfo:        Option[CatalogInfo],
  brightnessExpanded: View[IsExpanded],
  disabled:           Boolean
)(using Logger[IO])
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
    bandNormalizedAlignerOpt.flatMap(
      _.zoomOpt(
        SpectralDefinition.BandNormalized.sed[Surface].some,
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

  override val fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[Surface]]] =
    emissionLinesAlignerOpt.map(
      _.zoom(
        SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface],
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
  override protected val currentType: SpectralDefinition[Surface] => Option[SEDType[Surface]] =
    SurfaceSEDType.fromSpectralDefinition

  override protected val disabledItems: HashSet[SEDType[Surface]] =
    HashSet(SurfaceSEDType.UserDefinedType)

  override protected val brightnessEditor
    : (View[SortedMap[Band, BrightnessMeasure[Surface]]], View[IsExpanded], Boolean) => VdomNode =
    (brightnessesView, expanded, disabled) =>
      SurfaceBrightnessEditor(brightnessesView, expanded, disabled)

  override protected val emissionLineEditor
    : (View[SortedMap[Wavelength, EmissionLine[Surface]]], View[IsExpanded], Boolean) => VdomNode =
    (emissionLinesView, expanded, disabled) =>
      SurfaceEmissionLineEditor(emissionLinesView, expanded, disabled)

}
