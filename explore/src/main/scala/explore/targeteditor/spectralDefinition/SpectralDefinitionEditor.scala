// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import crystal.react.View
import explore.*
import explore.common.*
import explore.model.Attachment
import explore.utils.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.schemas.ObservationDB.Types.*

import scala.collection.immutable.SortedMap

private trait SpectralDefinitionEditor[T, S]:
  def spectralDefinition: Aligner[SpectralDefinition[T], S]
  def catalogInfo: Option[CatalogInfo]
  def calibrationRole: Option[CalibrationRole]
  def brightnessExpanded: View[IsExpanded]
  def customSedAttachments: List[Attachment]
  def disabled: Boolean

  def toInput: SpectralDefinition[T] => S
  def sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]]
  def bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]
  def emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]
  def fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[T]]]
