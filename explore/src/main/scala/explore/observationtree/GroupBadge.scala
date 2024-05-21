// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.GroupTree.Group
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*

case class GroupBadge(
  group:     Group,
  selected:  Boolean,
  onClickCB: ReactMouseEvent => Callback,
  href:      String,
  deleteCB:  Callback,
  isEmpty:   Boolean,
  readonly:  Boolean
) extends ReactFnProps(GroupBadge.component)

object GroupBadge:
  private type Props = GroupBadge

  private val component = ScalaFnComponent[Props]: props =>
    val group = props.group

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
        // Completely hidden for readonly mode
        .unless(props.readonly)

    <.a(
      ExploreStyles.ObsTreeGroupLeaf |+| ExploreStyles.SelectedGroupItem.when_(props.selected),
      ^.title     := group.id.show,
      ^.id        := show"obs-group-${group.id}",
      ^.draggable := false,
      ^.onClick ==> props.onClickCB,
      ^.href      := props.href
    )(
      if group.isAnd then "AND" else "OR",
      group.name.map(n => <.em(n.value)),
      deleteButton
    )
