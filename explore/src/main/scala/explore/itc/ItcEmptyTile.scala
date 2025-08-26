// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.primereact.Message

object ItcEmptyTile:
  val tile =
    Tile(
      ObsTabTileIds.ItcId.id,
      "ITC",
      ItcTileState.Empty,
      bodyClass = ExploreStyles.ItcTileBody
    )(
      _ =>
        Message(
          text = "Select an observing mode to view ITC results",
          severity = Message.Severity.Info
        ),
      (_, _) => EmptyVdom
    )
