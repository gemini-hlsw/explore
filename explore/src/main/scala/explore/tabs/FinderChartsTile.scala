// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.findercharts.FinderCharts
import explore.model.ObsAttachmentList
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

object FinderChartsTile:

  def finderChartsTile(
    programId:        Program.Id,
    obsAttachmentIds: View[SortedSet[ObsAttachment.Id]],
    authToken:        Option[NonEmptyString],
    obsAttachments:   View[ObsAttachmentList]
  ) =
    Tile(
      ObsTabTilesIds.FinderChartsId.id,
      s"Finder Charts",
      bodyClass = ExploreStyles.FinderChartsTile.some,
      canMinimize = true
    )(_ =>
      authToken
        .map(t => FinderCharts(programId, t, obsAttachmentIds, obsAttachments): VdomNode)
        .getOrElse(EmptyVdom)
    )
