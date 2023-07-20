// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import explore.UnderConstruction
import explore.attachments.ObsAttachmentsTable
import explore.components.Tile
import explore.components.TileController
import explore.model.AppContext
import explore.model.ExploreGridLayouts
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObsAttachmentList
import explore.model.ObsTabTilesIds
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.resizeDetector.hooks.*

case class OverviewTabContents(
  programId:                Program.Id,
  userVault:                Option[UserVault],
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  userPreferences:          UserPreferences
) extends ReactFnProps(OverviewTabContents.component)

object OverviewTabContents {
  private type Props = OverviewTabContents

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      .render { (props, _, resize) =>
        val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.OverviewLayout)
        val layouts        = props.userPreferences.overviewTabLayout

        val warningsAndErrorsTile = Tile(
          ObsTabTilesIds.WarningsAndErrorsId.id,
          "Warnings And Errors",
          none,
          canMinimize = true
        )(_ => UnderConstruction())

        val obsAttachmentsTile = Tile(
          ObsTabTilesIds.ObsAttachmentsId.id,
          "Observation Attachments",
          none,
          canMinimize = true
        )(renderInTitle =>
          Pot
            .fromOption(props.userVault)
            .renderPot(vault =>
              ObsAttachmentsTable(props.programId,
                                  vault.token,
                                  props.obsAttachments,
                                  props.obsAttachmentAssignments,
                                  renderInTitle
              )
            )
        )

        TileController(
          props.userVault.map(_.user.id),
          resize.width.getOrElse(1),
          defaultLayouts,
          layouts,
          List(warningsAndErrorsTile, obsAttachmentsTile),
          GridLayoutSection.OverviewLayout
        )
      }
}
