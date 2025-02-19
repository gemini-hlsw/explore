// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptyList
import cats.syntax.option.*
import crystal.Pot
import crystal.react.View
import explore.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConfigurationRequestList
import explore.model.ExploreGridLayouts
import explore.model.Observation
import explore.model.ObservationList
import explore.model.ProgramDetails
import explore.model.ProgramTabTileIds
import explore.model.ProgramTimes
import explore.model.ProgramUser
import explore.model.TargetList
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.programs.ProgramConfigRequestsTile
import explore.programs.ProgramDetailsTile
import explore.programs.ProgramNotesTile
import explore.programs.ProgramUnrequestedConfigsTile
import explore.users.AddProgramUserButton
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.ui.react.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class ProgramTabContents(
  programId:              Program.Id,
  programDetails:         View[ProgramDetails],
  configRequests:         View[ConfigurationRequestList],
  observations:           View[ObservationList],
  obs4ConfigRequests:     Map[ConfigurationRequest.Id, List[Observation]],
  configsWithoutRequests: Map[Configuration, NonEmptyList[Observation]],
  targets:                TargetList,
  userVault:              Option[UserVault],
  programTimes:           Pot[ProgramTimes],
  userPreferences:        UserPreferences,
  userIsReadonlyCoi:      Boolean,
  userIsPi:               Boolean
) extends ReactFnProps(ProgramTabContents):
  val users: View[List[ProgramUser]] = programDetails.zoom(ProgramDetails.allUsers)

object ProgramTabContents
    extends ReactFnComponent[ProgramTabContents](props =>
      for {
        ctx    <- useContext(AppContext.ctx)
        resize <- useResizeDetector
      } yield props.userVault.map: userVault =>
        val userId: Option[User.Id] = userVault.user.id.some

        val defaultLayouts: LayoutsMap =
          ExploreGridLayouts.sectionLayout(GridLayoutSection.ProgramsLayout)

        val layouts: LayoutsMap =
          props.userPreferences.programsTabLayout

        val detailsTile =
          Tile(
            ProgramTabTileIds.DetailsId.id,
            "Program Details"
          )(_ =>
            ProgramDetailsTile(
              props.programId,
              props.programDetails,
              props.programTimes,
              props.userIsReadonlyCoi
            )
          )

        val sharingRoles: Set[ProgramUserRole] =
          Set(ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.External)
        val countOfDataAccess                  =
          props.users.get.count(pu => sharingRoles.contains(pu.role) && pu.hasDataAccess)

        val dataUsersTile =
          Tile(
            ProgramTabTileIds.DataUsers.id,
            s"Data Users ($countOfDataAccess)"
          )(
            _ =>
              ProgramUsersTable(
                props.users,
                ProgramUsersTable.Mode.DataUsers(userVault)
              ),
            (_, _) =>
              <.div(
                ExploreStyles.AddProgramUserButton,
                Option
                  .when[VdomNode](props.userIsPi):
                    AddProgramUserButton(props.programId, ProgramUserRole.External, props.users)
                  .orEmpty,
                HelpIcon("program/data-users.md".refined)
              )
          )

        val notesTile =
          Tile(
            ProgramTabTileIds.NotesId.id,
            "Notes"
          )(_ => ProgramNotesTile())

        val configurationRequestsTile =
          Tile(
            ProgramTabTileIds.ChangeRequestsId.id,
            s"Requested Coordinates + Configurations + Constraints (${props.configRequests.get.size})",
            initialState = ProgramConfigRequestsTile.TileState.Empty
          )(
            ProgramConfigRequestsTile.Body(
              userId,
              props.programId,
              props.configRequests.get,
              props.obs4ConfigRequests,
              props.targets,
              _
            ),
            (s, _) =>
              ProgramConfigRequestsTile.Title(props.configRequests, props.userIsReadonlyCoi, s.get)
          )

        val unrequestedConfigsTile =
          Tile(
            ProgramTabTileIds.UnrequestedConfigsId.id,
            s"Unrequested Coordinates + Configurations + Constraints (${props.configsWithoutRequests.size})",
            initialState = ProgramUnrequestedConfigsTile.TileState.Empty
          )(
            ProgramUnrequestedConfigsTile.Body(
              userId,
              props.programId,
              props.configRequests,
              props.configsWithoutRequests,
              props.targets,
              _
            ),
            (s, _) =>
              ProgramUnrequestedConfigsTile
                .Title(props.configRequests, props.observations, props.userIsReadonlyCoi, s.get)
          )

        <.div(ExploreStyles.MultiPanelTile)(
          TileController(
            userId,
            resize.width.getOrElse(1),
            defaultLayouts,
            layouts,
            List(
              detailsTile,
              dataUsersTile,
              notesTile,
              configurationRequestsTile,
              unrequestedConfigsTile
            ),
            GridLayoutSection.ProgramsLayout
          )
        ).withRef(resize.ref)
    )
