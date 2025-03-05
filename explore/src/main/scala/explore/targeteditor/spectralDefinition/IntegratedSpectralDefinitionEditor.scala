// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.effect.IO
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import explore.*
import explore.common.*
import explore.model.enums.IntegratedSEDType
import explore.model.enums.IntegratedSEDType.given
import explore.model.enums.SedType
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

import brightnessesEditor.IntegratedBrightnessEditor
import emissionLineEditor.IntegratedEmissionLineEditor

case class IntegratedSpectralDefinitionEditor(
  spectralDefinition: Aligner[SpectralDefinition[Integrated], SpectralDefinitionIntegratedInput],
  catalogInfo:        Option[CatalogInfo],
  brightnessExpanded: View[IsExpanded],
  disabled:           Boolean,
  calibrationRole:    Option[CalibrationRole]
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
    : SpectralDefinition[Integrated] => Option[SedType[Integrated]] =
    IntegratedSEDType.fromSpectralDefinition

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
