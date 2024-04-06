// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AsterismIds
import explore.model.Execution
import explore.model.ObsTabTilesIds
import explore.model.OdbItcResult
import explore.model.syntax.all.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan

object SequenceEditorTile:

  def sequenceTile(
    programId:       Program.Id,
    obsId:           Observation.Id,
    obsExecution:    Pot[Execution],
    asterismIds:     AsterismIds,
    itc:             Option[OdbItcResult.Success],
    sequenceChanged: View[Pot[Unit]]
  ) =
    val control =
      obsExecution.orSpinner { execution =>
        val programTimeCharge = execution.programTimeCharge.value

        def timeDisplay(name: String, time: TimeSpan) =
          <.span(<.span(ExploreStyles.SequenceTileTitleItem)(name, ": "), time.toHoursMinutes)

        val executed = timeDisplay("Executed", programTimeCharge)

        execution.programTimeEstimate
          .map { plannedTime =>
            val total   = programTimeCharge +| plannedTime
            val pending = timeDisplay("Pending", plannedTime)
            val planned = timeDisplay("Planned", total)
            <.span(ExploreStyles.SequenceTileTitle)(
              planned,
              executed,
              pending
            )
          }
          .getOrElse(executed)
      }

    Tile(
      ObsTabTilesIds.SequenceId.id,
      s"Sequence",
      canMinimize = true,
      control = _ => control.some
    )(renderInTitle =>
      GeneratedSequenceViewer(
        programId,
        obsId,
        asterismIds.toList,
        itc.map(_.snPerClass).getOrElse(Map.empty),
        sequenceChanged
      )
    )
