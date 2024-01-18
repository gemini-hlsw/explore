// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.model.ObsTabTilesIds
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

object SequenceEditorTile:

  val sequenceTile =
    Tile(
      ObsTabTilesIds.SequenceId.id,
      s"Sequence",
      canMinimize = true
    )
