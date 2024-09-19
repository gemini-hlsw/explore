// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.option.*
import cats.effect.IO
import clue.*
import clue.data.Input
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.*
import explore.model.AppContext
import explore.model.CallForProposal
import explore.model.ExploreGridLayouts
import explore.model.ProgramTimeRange
import explore.model.ProgramUserWithRole
import explore.model.Proposal
import explore.model.ProposalAttachment
import explore.model.ProposalTabTileIds
import explore.model.UserInvitation
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.common.ProposalQueriesGQL
import explore.users.InviteUserButton
import lucuma.ui.react.given
import explore.users.ProgramUsersTable
import cats.data.NonEmptySet

case class ProposalEditor(
  programId:         Program.Id,
  optUserId:         Option[User.Id],
  proposal:          View[Proposal],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  users:             View[List[ProgramUserWithRole]],
  invitations:       View[List[UserInvitation]],
  attachments:       View[List[ProposalAttachment]],
  authToken:         Option[NonEmptyString],
  cfps:              List[CallForProposal],
  layout:            LayoutsMap,
  readonly:          Boolean
) extends ReactFnProps(ProposalEditor.component)

object ProposalEditor:
  private type Props = ProposalEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // .useStateViewBy((props, _, _, _) =>
      //   // mininum percent total time = need to preserve between class switches
      //   props.proposal
      //     .zoom(Proposal.proposalClass.andThen(ProposalClass.minPercentTotalTime))
      //     .get
      //     .getOrElse(IntPercent.unsafeFrom(80))
      // )
      // .useEffectWithDepsBy((props, _, _, _, _, _, _, _, _) => props.proposal.get.proposalClass)(
      //   // Deal with changes to the ProposalClass.
      //   (props, _, _, totalHours, minPct2, classType, _, _, oldClass) =>
      //     newClass => {
      //       val setClass =
      //         if (oldClass.get === newClass) Callback.empty
      //         else props.proposal.zoom(Proposal.proposalClass).set(newClass)
      //       val newType  = ProposalClassType.fromProposalClass(newClass)
      //       val setType  = if (classType.get === newType) Callback.empty else classType.set(newType)
      //       val newHours = ProposalClass.totalTime.getOption(newClass).map(toHours)
      //       val setHours = newHours
      //         .flatMap(h => if (h === totalHours.get) none else h.some)
      //         .foldMap(totalHours.set)
      //       val newPct2  = ProposalClass.minPercentTotalTime.getOption(newClass)
      //       val setPct2  =
      //         newPct2.flatMap(p => if (p === minPct2.get) none else p.some).foldMap(minPct2.set)
      //       setClass >> setType >> setHours >> setPct2
      //     }
      // )
      .useResizeDetector()
      .render: (props, ctx, resize) =>
        import ctx.given

        val undoCtx: UndoContext[Proposal] = UndoContext(props.undoStacks, props.proposal)

        val aligner: Aligner[Proposal, ProposalPropertiesInput] =
          Aligner(
            undoCtx,
            UpdateProposalInput(
              programId = props.programId.assign,
              SET = ProposalPropertiesInput()
            ),
            (ProposalQueriesGQL.UpdateProposalMutation[IO].execute(_)).andThen(_.void)
          ).zoom(Iso.id[Proposal].asLens, UpdateProposalInput.SET.modify)

        val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
          aligner.zoom(Proposal.abstrakt, ProposalPropertiesInput.`abstract`.modify)

        val abstractView: View[Option[NonEmptyString]] = abstractAligner.view(_.orUnassign)

        val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

        val detailsTile =
          Tile(ProposalTabTileIds.DetailsId.id, "Details")(
            _ =>
              ProposalDetailsBody(
                props.proposal,
                aligner,
                props.timeEstimateRange,
                props.cfps,
                props.readonly
              ),
            (_, s) => ProposalDetailsTitle(undoCtx, s)
          )

        val usersTile =
          Tile(
            ProposalTabTileIds.UsersId.id,
            "Investigators"
          )(
            _ =>
              ProgramUsersTable(
                props.programId,
                props.users,
                props.invitations,
                NonEmptySet.of(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.CoiRO),
                props.readonly
              ),
            (_, _) =>
              Option
                .unless[VdomNode](props.readonly):
                  InviteUserButton(
                    props.programId,
                    ProgramUserRole.Coi,
                    props.invitations
                  )
                .orEmpty
          )

        val abstractTile =
          Tile(
            ProposalTabTileIds.AbstractId.id,
            "Abstract",
            bodyClass = ExploreStyles.ProposalAbstract
          )(_ =>
            FormInputTextAreaView(
              id = "abstract".refined,
              value = abstractView.as(OptionNonEmptyStringIso)
            )(^.disabled := props.readonly)
          )

        val attachmentsTile =
          Tile(ProposalTabTileIds.AttachmentsId.id, "Attachments")(_ =>
            props.authToken.map(token =>
              ProposalAttachmentsTable(props.programId, token, props.attachments, props.readonly)
            )
          )

        <.div(ExploreStyles.MultiPanelTile)(
          TileController(
            props.optUserId,
            resize.width.getOrElse(1),
            defaultLayouts,
            props.layout,
            List(detailsTile, usersTile, abstractTile, attachmentsTile),
            GridLayoutSection.ProposalLayout,
            storeLayout = true
          )
        ).withRef(resize.ref)
