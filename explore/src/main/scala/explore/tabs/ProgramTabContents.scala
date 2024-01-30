// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import explore.*
import explore.components.Tile
import explore.components.TileController
import explore.model.AppContext
import explore.model.ExploreGridLayouts
import explore.model.ProgramTabTileIds
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.programs.ProgramChangeRequestsTile
import explore.programs.ProgramDetailsTile
import explore.programs.ProgramNotesTile
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class ProgramTabContents(
  programId:         Program.Id,
  userVault:         Option[UserVault],
  timeEstimateRange: Option[CategorizedTimeRange],
  timeCharge:        CategorizedTime,
  userPreferences:   UserPreferences
) extends ReactFnProps(ProgramTabContents.component)

object ProgramTabContents:

  private type Props = ProgramTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useResizeDetector()
    .render { (props, _, resize) =>
      val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProgramsLayout)

      val layouts = props.userPreferences.programsTabLayout

      val detailsTile = Tile(
        ProgramTabTileIds.DetailsId.id,
        "Program Details",
        canMinimize = true
      )(_ => ProgramDetailsTile(props.timeEstimateRange, props.timeCharge))
      // )(_ => optPropInfo.renderPotOption(optProp => ProgramDetailsTile(props.timeEstimateRange, props.timeCharge))

      val notesTile = Tile(
        ProgramTabTileIds.NotesId.id,
        "Notes",
        canMinimize = true
      )(_ => ProgramNotesTile())

      val changeRequestsTile = Tile(
        ProgramTabTileIds.ChangeRequestsId.id,
        "Change Requests",
        canMinimize = true
      )(_ => ProgramChangeRequestsTile())

      <.div(
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

    }
