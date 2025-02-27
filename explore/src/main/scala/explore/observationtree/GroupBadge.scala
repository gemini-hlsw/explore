// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.Group
import explore.model.enums.GroupWarning
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*

case class GroupBadge(
  group:     Group,
  warnings:  Option[NonEmptySet[GroupWarning]],
  selected:  Boolean,
  onClickCB: ReactMouseEvent => Callback,
  href:      String,
  deleteCB:  Callback,
  isEmpty:   Boolean,
  readonly:  Boolean
) extends ReactFnProps(GroupBadge.component):
  val isDisabled: Boolean = readonly || group.system

object GroupBadge:
  private type Props = GroupBadge

  private val component = ScalaFnComponent[Props]: props =>
    val group = props.group
    val name  = group.name.foldMap(_.value.toLowerCase())

    val warningIcon = props.warnings.map(nes =>
      <.span(Icons.MissingInfoIcon)
        .withTooltip(
          if (nes.length === 1) nes.head.shortMsg
          else
            <.ul(nes.toList.toTagMod(w => <.li(w.shortMsg)))
        )
    )

    val deleteButton =
      <.span(
        Button(
          text = true,
          clazz = ExploreStyles.DeleteButton,
          icon = Icons.Trash,
          tooltip = "Delete",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB,
          disabled = !props.isEmpty
        ).small
          // Tooltip is shown only if the group is not empty
      ).withTooltipUnless(props.isEmpty, "Empty group before deleting", Placement.Right)
        // Completely hidden when disabled
        .unless(props.isDisabled)

    <.a(
      ExploreStyles.ObsTreeGroupLeaf |+| ExploreStyles.SelectedGroupItem.when_(props.selected),
      ^.title     := group.id.show,
      ^.id        := show"obs-group-${group.id}",
      ^.draggable := false,
      ^.onClick ==> props.onClickCB,
      ^.href      := props.href
    )(
      <.div(
        ExploreStyles.GroupBadgeLeft,
        if group.system then "" else if group.isAnd then "AND" else "OR",
        group.name.map(n => <.em(n.value))
      ),
      <.div(
        ExploreStyles.GroupBadgeRight,
        warningIcon,
        deleteButton,
        NonEmptyString
          .from(s"groups/system-$name.md")
          .toOption
          .map(HelpIcon(_))
          .when(group.system)
      )
    )
