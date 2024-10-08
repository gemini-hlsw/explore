// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.findercharts.ChartSelector
import explore.findercharts.FinderChartsBody
import explore.findercharts.FinderChartsTileState
import explore.findercharts.FinderChartsTitle
import explore.model.ObsAttachmentList
import explore.model.ObsTabTilesIds
import explore.model.Observation
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.Program
import lucuma.ui.react.given

import scala.collection.immutable.SortedSet

object FinderChartsTile:

  def finderChartsTile(
    programId:        Program.Id,
    oid:              Observation.Id,
    obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
    authToken:        Option[NonEmptyString],
    obsAttachments:   View[ObsAttachmentList],
    parallacticAngle: Option[Angle],
    readOnly:         Boolean
  ) =
    Tile(
      ObsTabTilesIds.FinderChartsId.id,
      s"Finder Charts",
      FinderChartsTileState(ChartSelector.Closed, None),
      bodyClass = ExploreStyles.FinderChartsTile
    )(
      s =>
        authToken
          .map[VdomNode](t =>
            FinderChartsBody(programId,
                             oid,
                             t,
                             obsAttachmentIds,
                             obsAttachments,
                             parallacticAngle,
                             readOnly,
                             s
            )
          )
          .orEmpty,
      (s, _) =>
        authToken
          .map[VdomNode](t =>
            FinderChartsTitle(programId, t, obsAttachmentIds, obsAttachments, readOnly)(s)
          )
    )
