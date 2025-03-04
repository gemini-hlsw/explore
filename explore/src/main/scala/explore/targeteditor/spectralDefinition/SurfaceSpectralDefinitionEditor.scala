// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import coulomb.*
import coulomb.units.si.Kelvin
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.*
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.AppContext
import explore.model.display.given
import explore.model.enums.IntegratedSEDType
import explore.model.enums.IntegratedSEDType.given
import explore.model.enums.SedType
import explore.model.enums.SurfaceSEDType
import explore.model.enums.SurfaceSEDType.given
import explore.model.syntax.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
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
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import brightnessesEditor.SurfaceBrightnessEditor
import emissionLineEditor.SurfaceEmissionLineEditor

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

case class SurfaceSpectralDefinitionEditor(
  spectralDefinition: Aligner[SpectralDefinition[Surface], SpectralDefinitionSurfaceInput],
  catalogInfo:        Option[CatalogInfo],
  brightnessExpanded: View[IsExpanded],
  disabled:           Boolean,
  calibrationRole:    Option[CalibrationRole]
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
  override protected val currentType: SpectralDefinition[Surface] => Option[SedType[Surface]] =
    SurfaceSEDType.fromSpectralDefinition

  override protected val disabledItems: HashSet[SedType[Surface]] =
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
