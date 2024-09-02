// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import crystal.Pot
import crystal.react.View
import explore.components.Tile
import explore.model.AsterismIds
import explore.model.Execution
import explore.model.ObsTabTilesIds
import explore.model.Observation
import lucuma.core.model.Program

object SequenceEditorTile:

  def sequenceTile(
    programId:       Program.Id,
    obsId:           Observation.Id,
    obsExecution:    Pot[Execution],
    asterismIds:     AsterismIds,
    sequenceChanged: View[Pot[Unit]]
  ) =
    Tile(
      ObsTabTilesIds.SequenceId.id,
      s"Sequence"
    )(
      _ =>
        GeneratedSequenceBody(
          programId,
          obsId,
          asterismIds.toList,
          sequenceChanged
        ),
      (_, _) => GeneratedSequenceTitle(obsExecution)
    )
