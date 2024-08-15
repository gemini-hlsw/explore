// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.attachments.ObsAttachmentsTable
import explore.components.Tile
import explore.components.TileController
import explore.model.AppContext
import explore.model.ExploreGridLayouts
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObsAttachmentList
import explore.model.ObsTabTilesIds
import explore.model.ObservationList
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.syntax.ui.*
import explore.validations.ObservationValidationsTable
import explore.validations.ObservationValidationsTableState
import explore.validations.ObservationValidationsTableControls
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class OverviewTabContents(
  programId:                Program.Id,
  userVault:                Option[UserVault],
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  observations:             ObservationList,
  layout:                   LayoutsMap,
  readonly:                 Boolean
) extends ReactFnProps(OverviewTabContents.component)

object OverviewTabContents {
  private type Props = OverviewTabContents

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      .useStateView(none[Boolean => Callback])
      .render { (props, _, resize, cb) =>
        val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.OverviewLayout)

        val warningsAndErrorsTile = Tile(
          ObsTabTilesIds.WarningsAndErrorsId.id,
          ObservationValidationsTableState(props.programId, props.observations, cb),
          "Warnings And Errors",
          none,
          canMinimize = true
        )(ObservationValidationsTable.apply, ObservationValidationsTableControls.apply)

        // val obsAttachmentsTile = Tile(
        //   ObsTabTilesIds.ObsAttachmentsId.id,
        //   "Observation Attachments",
        //   none,
        //   canMinimize = true
        // )(renderInTitle =>
        //   props.userVault.map(vault =>
        //     ObsAttachmentsTable(props.programId,
        //                         vault.token,
        //                         props.obsAttachments,
        //                         props.obsAttachmentAssignments,
        //                         props.readonly,
        //                         renderInTitle
        //     )
        //   )
        // )

        <.div(
          TileController(
            props.userVault.map(_.user.id),
            resize.width.getOrElse(1),
            defaultLayouts,
            props.layout,
            List(
              warningsAndErrorsTile.some
                // if (!props.userVault.isGuest) obsAttachmentsTile.some else none
            ).flattenOption,
            GridLayoutSection.OverviewLayout
          )
        ).withRef(resize.ref)
      }
}
