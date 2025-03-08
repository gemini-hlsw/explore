// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.effect.IO
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.*
import explore.model.AttachmentList
import explore.model.enums.SedType
import explore.model.enums.SurfaceSedType
import explore.model.enums.SurfaceSedType.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.Program
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap

import brightnessesEditor.SurfaceBrightnessEditor
import emissionLineEditor.SurfaceEmissionLineEditor

case class SurfaceSpectralDefinitionEditor(
  programId:          Program.Id,
  spectralDefinition: Aligner[SpectralDefinition[Surface], SpectralDefinitionSurfaceInput],
  catalogInfo:        Option[CatalogInfo],
  brightnessExpanded: View[IsExpanded],
  attachments:        View[AttachmentList],
  authToken:          Option[NonEmptyString],
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
    SurfaceSedType.fromSpectralDefinition

  override protected val userDefinedType: SedType[Surface] = SurfaceSedType.UserDefinedType

  override protected val brightnessEditor
    : (View[SortedMap[Band, BrightnessMeasure[Surface]]], View[IsExpanded], Boolean) => VdomNode =
    (brightnessesView, expanded, disabled) =>
      SurfaceBrightnessEditor(brightnessesView, expanded, disabled)

  override protected val emissionLineEditor
    : (View[SortedMap[Wavelength, EmissionLine[Surface]]], View[IsExpanded], Boolean) => VdomNode =
    (emissionLinesView, expanded, disabled) =>
      SurfaceEmissionLineEditor(emissionLinesView, expanded, disabled)
}
