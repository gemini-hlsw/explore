// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.attachments.ObsAttachmentsTableBody
import explore.attachments.ObsAttachmentsTableTileState
import explore.attachments.ObsAttachmentsTableTitle
import explore.components.Tile
import explore.components.TileController
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.ExploreGridLayouts
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObsTabTileIds
import explore.model.ObservationList
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.validations.ObservationValidationsTableBody
import explore.validations.ObservationValidationsTableTileState
import explore.validations.ObservationValidationsTableTitle
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class OverviewTabContents(
  programId:                Program.Id,
  userVault:                Option[UserVault],
  attachments:              View[AttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  observations:             View[ObservationList],
  layout:                   LayoutsMap,
  proposalIsAccepted:       Boolean,
  readonly:                 Boolean
) extends ReactFnProps(OverviewTabContents.component):
  val userId: Option[User.Id] = userVault.map(_.user.id)

object OverviewTabContents {
  private type Props = OverviewTabContents

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      .render { (props, _, resize) =>
        val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.OverviewLayout)

        val warningsAndErrorsTile = Tile(
          ObsTabTileIds.WarningsAndErrorsId.id,
          "Warnings And Errors",
          ObservationValidationsTableTileState(_ => Callback.empty)
        )(
          ObservationValidationsTableBody(props.userId, props.programId, props.observations, _),
          ObservationValidationsTableTitle.apply
        )

        val obsAttachmentsTile = props.userVault
          .flatMap(vault =>
            if (props.proposalIsAccepted)
              Tile(
                ObsTabTileIds.ObsAttachmentsId.id,
                "Observation Attachments",
                ObsAttachmentsTableTileState()
              )(
                ObsAttachmentsTableBody(props.programId,
                                        vault.token,
                                        props.obsAttachmentAssignments,
                                        props.attachments,
                                        props.readonly,
                                        _
                ),
                (s, _) =>
                  ObsAttachmentsTableTitle(props.programId,
                                           vault.token,
                                           props.attachments,
                                           props.readonly,
                                           s
                  )
              ).some
            else None
          )
          // provide a hidden dummy tile to not mess up the saved layouts.
          .getOrElse(
            Tile(ObsTabTileIds.ObsAttachmentsId.id, "", hidden = true)(_ => EmptyVdom)
          )

        <.div(
          TileController(
            props.userId,
            resize.width.getOrElse(1),
            defaultLayouts,
            props.layout,
            List(
              warningsAndErrorsTile,
              obsAttachmentsTile
            ),
            GridLayoutSection.OverviewLayout
          )
        ).withRef(resize.ref)
      }
}
