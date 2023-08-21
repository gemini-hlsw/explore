// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import crystal.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.model.ObsTabTilesIds
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.syntax.all.given

object SequenceEditorTile:

  def sequenceTile(
    programId:       Program.Id,
    obsId:           Observation.Id,
    targetIds:       List[Target.Id],
    snPerClass:      Map[ObserveClass, SignalToNoise],
    sequenceChanged: View[Pot[Unit]]
  ) =
    Tile(
      ObsTabTilesIds.SequenceId.id,
      s"Sequence",
      canMinimize = true
    )(_ => GeneratedSequenceViewer(programId, obsId, targetIds, snPerClass, sequenceChanged))
