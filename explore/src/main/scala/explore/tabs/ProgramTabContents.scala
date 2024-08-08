// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import crystal.Pot
import explore.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreGridLayouts
import explore.model.ProgramTabTileIds
import explore.model.ProgramTimes
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.programs.ProgramChangeRequestsTile
import explore.programs.ProgramDetailsTile
import explore.programs.ProgramNotesTile
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import explore.model.ProgramDetails
import lucuma.core.model.Semester

case class ProgramTabContents(
  programDetails:  ProgramDetails,
  userVault:       Option[UserVault],
  programTimes:    Pot[ProgramTimes],
  semester:        Semester,
  userPreferences: UserPreferences
) extends ReactFnProps(ProgramTabContents.component)

object ProgramTabContents:
  private type Props = ProgramTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useResizeDetector()
    .render: (props, _, resize) =>
      val defaultLayouts: LayoutsMap =
        ExploreGridLayouts.sectionLayout(GridLayoutSection.ProgramsLayout)

      val layouts: LayoutsMap =
        props.userPreferences.programsTabLayout

      val detailsTile =
        Tile(
          ProgramTabTileIds.DetailsId.id,
          "Program Details"
        )(_ => ProgramDetailsTile(props.programDetails, props.programTimes, props.semester))

      val notesTile =
        Tile(
          ProgramTabTileIds.NotesId.id,
          "Notes"
        )(_ => ProgramNotesTile())

      val changeRequestsTile =
        Tile(
          ProgramTabTileIds.ChangeRequestsId.id,
          "Change Requests"
        )(_ => ProgramChangeRequestsTile())

      <.div(ExploreStyles.MultiPanelTile)(
        TileController(
          props.userVault.map(_.user.id),
          resize.width.getOrElse(1),
          defaultLayouts,
          layouts,
          List(
            detailsTile,
            notesTile,
            changeRequestsTile
          ),
          GridLayoutSection.ProgramsLayout
        )
      ).withRef(resize.ref)
