// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import clue.data.syntax.*
import crystal.react.hooks.*
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
import explore.proposal.ProposalInfo
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ProgramQueriesGQL.ProgramEditSubscription
import queries.common.ProgramQueriesGQL.ProgramProposalQuery

case class ProgramTabContents(
  programId:       Program.Id,
  userVault:       Option[UserVault],
  userPreferences: UserPreferences
) extends ReactFnProps(ProgramTabContents.component)

object ProgramTabContents:

  private type Props = ProgramTabContents

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useResizeDetector()
    .useStreamResourceViewOnMountBy { (props, ctx, _) =>
      import ctx.given

      ProgramProposalQuery[IO]
        .query(props.programId)
        .map(data =>
          data.program
            .map(prog =>
              ProposalInfo(prog.proposal,
                           prog.timeEstimateRange.map(_.minimum.total),
                           prog.timeEstimateRange.map(_.maximum.total),
                           prog.pi,
                           prog.users,
                           prog.timeCharge
              )
            )
        )
        .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO](props.programId.assign))
    }
    .render { (props, _, resize, optPropInfo) =>
      val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProgramsLayout)

      val layouts = props.userPreferences.programsTabLayout

      val detailsTile = Tile(
        ProgramTabTileIds.DetailsId.id,
        "Program Details",
        canMinimize = true
      )(_ => optPropInfo.renderPotOption(optProp => ProgramDetailsTile(optProp.get)))

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
