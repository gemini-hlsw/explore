// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.attachments.Action
import explore.attachments.ObsAttachmentUtils
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.schemas.enums.ObsAttachmentType
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

trait FinderChartsAttachmentUtils:
  def validAttachments(
    allAttachments:   ObsAttachmentList,
    obsAttachmentIds: SortedSet[ObsAtt.Id]
  ): ObsAttachmentList =
    allAttachments.filter { case (_, attachment) =>
      (attachment.attachmentType === ObsAttachmentType.Finder) && obsAttachmentIds
        .contains(attachment.id)
    }

object FinderChartsAttachmentUtils extends FinderChartsAttachmentUtils

object ChartSelector extends NewType[Boolean]:
  inline def Open: ChartSelector   = ChartSelector(true)
  inline def Closed: ChartSelector = ChartSelector(false)
  extension (s: ChartSelector)
    def flip: ChartSelector =
      if (s.value) ChartSelector.Closed else ChartSelector.Open

type ChartSelector = ChartSelector.Type

def finderChartsSelector(
  obsAttachments:   ObsAttachmentList,
  obsAttachmentIds: SortedSet[ObsAtt.Id],
  selected:         View[Option[ObsAtt.Id]]
): VdomNode =
  FormDropdownOptional(
    id = "attachment-selector".refined,
    placeholder = if (obsAttachmentIds.nonEmpty) "Select finder chart" else "No finder charts",
    options = FinderChartsAttachmentUtils
      .validAttachments(obsAttachments, obsAttachmentIds)
      .map(_._2)
      .map { attachment =>
        new SelectItem[ObsAttachment](value = attachment, label = attachment.fileName.value)
      }
      .toList,
    value = obsAttachments.find(i => selected.get.exists(_ === i._2.id)).map(_._2),
    onChange = (att: Option[ObsAttachment]) => selected.set(att.map(_.id))
  )

def attachmentSelector(
  programId:        Program.Id,
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   View[ObsAttachmentList],
  ctx:              AppContext[IO],
  client:           OdbRestClient[IO],
  selected:         View[Option[ObsAtt.Id]],
  action:           View[Action],
  added:            UseState[Option[ObsAtt.Id]],
  chartSelector:    View[ChartSelector],
  readOnly:         Boolean
): VdomNode = {
  import ctx.given

  def addNewFinderChart(e: ReactEventFromInput) =
    action.set(Action.Insert) *>
      ObsAttachmentUtils.onInsertFileSelected(
        programId,
        obsAttachments,
        ObsAttachmentType.Finder,
        client,
        action,
        id => obsAttachmentIds.mod(_ + id) *> added.setState(id.some) *> selected.set(id.some)
      )(e)

  <.div(
    ExploreStyles.FinderChartsSelectorSection,
    Button(
      severity = Button.Severity.Secondary,
      outlined = chartSelector.get.value,
      icon = Icons.Link.withFixedWidth(false).withInverse(chartSelector.get.value),
      onClick = chartSelector.mod(_.flip),
      tooltip = s"Select charts"
    ).tiny.compact,
    <.label(
      ObsAttachmentUtils.LabelButtonClasses,
      ^.htmlFor := "attachment-upload",
      Icons.FileArrowUp.withFixedWidth(true)
    ).withTooltip(
      tooltip = s"Upload new finder chart",
      placement = Placement.Right
    ).when(!readOnly),
    <.input(
      ExploreStyles.FileUpload,
      ^.tpe    := "file",
      ^.onChange ==> addNewFinderChart,
      ^.id     := "attachment-upload",
      ^.name   := "file",
      ^.accept := ObsAttachmentType.Finder.accept
    ).when(!readOnly),
    finderChartsSelector(obsAttachments.get, obsAttachmentIds.get, selected)
  )
}
