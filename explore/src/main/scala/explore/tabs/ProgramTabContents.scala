// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import explore.*
import explore.components.Tile
import explore.components.TileController
import explore.model.ExploreGridLayouts
import explore.model.ProgramTabTileIds
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Program
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.resizeDetector.hooks.*

case class ProgramTabContents(
  programId:       Program.Id,
  userVault:       Option[UserVault],
  userPreferences: UserPreferences
) extends ReactFnProps(ProgramTabContents.component)

object ProgramTabContents:

  private type Props = ProgramTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useResizeDetector()
    .render { (props, resize) =>
      val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProgramsLayout)

      val layouts = props.userPreferences.programsTabLayout

      val detailsTile = Tile(
        ProgramTabTileIds.DetailsId.id,
        "Program Details",
        canMinimize = true
      )(_ => UnderConstruction())

      val notesTile = Tile(
        ProgramTabTileIds.NotesId.id,
        "Notes",
        canMinimize = true
      )(_ => UnderConstruction())

      val changeRequestsTile = Tile(
        ProgramTabTileIds.ChangeRequestsId.id,
        "Change Requests",
        canMinimize = true
      )(_ => UnderConstruction())

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

    }
