// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.all.*
import lucuma.react.common.*
import lucuma.react.common.style.Css
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.PopupMenuRef

case class ScienceBandPopupMenu(
  currentBand:           Option[ScienceBand],
  allocatedScienceBands: NonEmptySet[ScienceBand],
  onSelect:              ScienceBand => Callback,
  menuRef:               PopupMenuRef
) extends ReactFnProps(ScienceBandPopupMenu.component):
  val menuItems = allocatedScienceBands.toNonEmptyList.toList.map: b =>
    val isCurrent = currentBand.exists(_ === b)
    // We need to always have an icon to keep the items consistent.
    val iconClass = if (isCurrent) Css.Empty else ExploreStyles.Hidden
    MenuItem.Item(
      icon = Icons.Checkmark.withClass(iconClass),
      label = b.longName,
      command = if (isCurrent) Callback.empty else onSelect(b)
    )

object ScienceBandPopupMenu:
  private type Props = ScienceBandPopupMenu

  private val component =
    ScalaFnComponent[Props]: p =>
      PopupMenu(model = p.menuItems, clazz = ExploreStyles.ScienceBandPopupMenu)
        .withRef(p.menuRef.ref)
