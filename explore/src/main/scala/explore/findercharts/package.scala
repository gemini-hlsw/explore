// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.FileUploadButton
import explore.components.ui.ExploreStyles
import explore.model.Attachment
import explore.model.AttachmentList
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.core.util.NewBoolean
import lucuma.react.primereact.Button
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

trait FinderChartsAttachmentUtils:
  def validAttachments(
    allAttachments:   AttachmentList,
    obsAttachmentIds: SortedSet[Attachment.Id]
  ): AttachmentList =
    allAttachments.filter { case (_, attachment) =>
      (attachment.attachmentType === AttachmentType.Finder) && obsAttachmentIds
        .contains(attachment.id)
    }

object FinderChartsAttachmentUtils extends FinderChartsAttachmentUtils

object ChartSelector extends NewBoolean { inline def Open = True; inline def Closed = False }
type ChartSelector = ChartSelector.Type

def finderChartsSelector(
  obsAttachments:   AttachmentList,
  obsAttachmentIds: SortedSet[Attachment.Id],
  selected:         View[Option[Attachment.Id]]
): VdomNode =
  FormDropdownOptional(
    id = "attachment-selector".refined,
    placeholder = if (obsAttachmentIds.nonEmpty) "Select finder chart" else "No finder charts",
    options = FinderChartsAttachmentUtils
      .validAttachments(obsAttachments, obsAttachmentIds)
      .map(_._2)
      .map { attachment =>
        new SelectItem[Attachment](value = attachment, label = attachment.fileName.value)
      }
      .toList,
    value = obsAttachments.find(i => selected.get.exists(_ === i._2.id)).map(_._2),
    onChange = (att: Option[Attachment]) => selected.set(att.map(_.id))
  )

def attachmentSelector(
  programId:     Program.Id,
  attachmentIds: View[SortedSet[Attachment.Id]],
  attachments:   View[AttachmentList],
  authToken:     Option[NonEmptyString],
  selected:      View[Option[Attachment.Id]],
  added:         UseState[Option[Attachment.Id]],
  chartSelector: View[ChartSelector],
  readOnly:      Boolean
): VdomNode =
  <.div(ExploreStyles.FinderChartsSelectorSection)(
    Button(
      severity = Button.Severity.Secondary,
      outlined = chartSelector.get.value,
      icon = Icons.Link.withFixedWidth(false).withInverse(chartSelector.get.value),
      onClick = chartSelector.mod(_.flip),
      tooltip = s"Select charts"
    ).tiny.compact,
    FileUploadButton(
      programId,
      attachments,
      AttachmentType.Finder,
      id => attachmentIds.mod(_ + id) *> added.setState(id.some) *> selected.set(id.some),
      disabled = readOnly,
      authToken = authToken
    ),
    finderChartsSelector(attachments.get, attachmentIds.get, selected)
  )
