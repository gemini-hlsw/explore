// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.*
import explore.model.Attachment
import explore.model.AttachmentList
import explore.utils.*
import japgolly.scalajs.react.Callback
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.Program
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.schemas.ObservationDB.Types.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap

private trait SpectralDefinitionEditor[T, S]:
  def programId: Program.Id
  def spectralDefinition: Aligner[SpectralDefinition[T], S]
  def catalogInfo: Option[CatalogInfo]
  def calibrationRole: Option[CalibrationRole]
  def brightnessExpanded: View[IsExpanded]
  def attachments: View[AttachmentList]
  def authToken: Option[NonEmptyString]
  def disabled: Boolean

  def toInput: SpectralDefinition[T] => S
  def sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]]
  def bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]
  def emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]
  def fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[T]]]

  protected[spectralDefinition] val currentCustomSedAttachmentId: Option[Attachment.Id] =
    SpectralDefinition.unnormalizedSED.some
      .andThen(UnnormalizedSED.userDefinedAttachment)
      .andThen(UnnormalizedSED.UserDefinedAttachment.attachmentId)
      .getOption(spectralDefinition.get)

  protected[spectralDefinition] val customSedAttachments: List[Attachment] =
    attachments.get.map(_._2).toList.filter(_.attachmentType === AttachmentType.CustomSED)

  protected[spectralDefinition] val modSpectralDefinition
    : Logger[IO] ?=> Endo[SpectralDefinition[T]] => Callback =
    spectralDefinition.view(toInput).mod(_)
