// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.findercharts.FinderCharts
import explore.findercharts.finderChartsSelector
import explore.model.ObsAttachmentList
import explore.model.ObsTabTilesIds
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

object FinderChartsTile:

  def finderChartsTile(
    programId:        Program.Id,
    oid:              Observation.Id,
    obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
    authToken:        Option[NonEmptyString],
    obsAttachments:   View[ObsAttachmentList],
    selected:         View[Option[ObsAtt.Id]],
    parallacticAngle: Option[Angle]
  ) =
    val control = <.div(ExploreStyles.JustifiedEndTileControl,
                        finderChartsSelector(obsAttachments.get, obsAttachmentIds.get, selected)
    )

    Tile(
      ObsTabTilesIds.FinderChartsId.id,
      s"Finder Charts",
      bodyClass = ExploreStyles.FinderChartsTile,
      canMinimize = true,
      renderInTitleClass = ExploreStyles.FinderChartsInTitle,
      control = state => Some(control).filter(_ => state.isMinimized)
    )(renderInTitle =>
      authToken
        .map(t =>
          FinderCharts(programId,
                       oid,
                       t,
                       obsAttachmentIds,
                       obsAttachments,
                       selected,
                       parallacticAngle,
                       renderInTitle
          ): VdomNode
        )
        .getOrElse(EmptyVdom)
    )
