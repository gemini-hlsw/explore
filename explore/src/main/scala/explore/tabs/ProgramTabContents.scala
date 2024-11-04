// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.option.*
import crystal.Pot
import crystal.react.View
import explore.*
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
import explore.model.TargetList
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.programs.ProgramConfigRequestsTile
import explore.programs.ProgramDetailsTile
import explore.programs.ProgramNotesTile
import explore.programs.ProgramUnrequestedConfigsTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
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
  semester:               Semester,
  userPreferences:        UserPreferences
) extends ReactFnProps(ProgramTabContents.component)

object ProgramTabContents:
  private type Props = ProgramTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useResizeDetector()
    .render: (props, _, resize) =>
      props.userVault.map: userVault =>
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
              props.semester,
              userVault.user.role.access
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
            s"Requested Coordinates + Configurations + Constraints (${props.configRequests.get.size})"
          )(_ =>
            ProgramConfigRequestsTile(
              props.programId,
              props.configRequests.get,
              props.obs4ConfigRequests,
              props.targets
            )
          )

        val unrequestedConfigsTile =
          Tile(
            ProgramTabTileIds.UnrequestedConfigsId.id,
            s"Unrequested Coordinates + Configurations + Constraints (${props.configsWithoutRequests.size})",
            initialState = ProgramUnrequestedConfigsTable.TileState.Empty
          )(
            ProgramUnrequestedConfigsTable.Body(
              props.programId,
              props.configRequests,
              props.configsWithoutRequests,
              props.targets,
              _
            ),
            (s, _) =>
              ProgramUnrequestedConfigsTable.Title(props.configRequests, props.observations, s.get)
          )

        <.div(ExploreStyles.MultiPanelTile)(
          TileController(
            userVault.user.id.some,
            resize.width.getOrElse(1),
            defaultLayouts,
            layouts,
            List(
              detailsTile,
              notesTile,
              configurationRequestsTile,
              unrequestedConfigsTile
            ),
            GridLayoutSection.ProgramsLayout
          )
        ).withRef(resize.ref)
