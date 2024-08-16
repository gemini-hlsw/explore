// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order.*
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
import explore.model.CoIInvitation
import explore.model.ExploreGridLayouts
import explore.model.ProgramTimeRange
import explore.model.ProgramUserWithRole
import explore.model.Proposal
import explore.model.ProposalAttachment
import explore.model.ProposalTabTileIds
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.hooks.UseOverlayPanelRef.implicits.*
import lucuma.react.resizeDetector.UseResizeDetectorReturn
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

case class ProposalEditor(
  programId:         Program.Id,
  optUserId:         Option[User.Id],
  proposal:          View[Proposal],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  users:             View[List[ProgramUserWithRole]],
  invitations:       View[List[CoIInvitation]],
  attachments:       View[List[ProposalAttachment]],
  authToken:         Option[NonEmptyString],
  cfps:              List[CallForProposal],
  layout:            LayoutsMap,
  readonly:          Boolean
) extends ReactFnProps(ProposalEditor.component)

object ProposalEditor:
  private type Props = ProposalEditor

  private def renderFn(
    props:  Props,
    resize: UseResizeDetectorReturn,
    ref:    OverlayPanelRef
  )(using ctx: AppContext[IO]) = {
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

    val abstractView = abstractAligner.view(_.orUnassign)

    val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

    val detailsTile =
      Tile(ProposalTabTileIds.DetailsId.id, (), "Details")(
        _ =>
          ProposalDetailsBody(props.proposal,
                              aligner,
                              props.timeEstimateRange,
                              props.cfps,
                              props.readonly
          ),
        (_, s) => ProposalDetailsTitle(undoCtx)(s)
      )

    val usersTile =
      ProgramUsers.programUsersTile(props.programId,
                                    props.readonly,
                                    props.users,
                                    props.invitations,
                                    ref
      )

    val abstractTile =
      Tile(ProposalTabTileIds.AbstractId.id,
           (),
           "Abstract",
           canMinimize = true,
           bodyClass = ExploreStyles.ProposalAbstract
      )(_ =>
        FormInputTextAreaView(
          id = "abstract".refined,
          value = abstractView.as(OptionNonEmptyStringIso)
        )(^.disabled := props.readonly)
      )

    val attachmentsTile =
      Tile(ProposalTabTileIds.AttachmentsId.id, (), "Attachments", canMinimize = true)(_ =>
        props.authToken.map(token =>
          ProposalAttachmentsTable(props.programId, token, props.attachments, props.readonly)
        )
      )

    <.div(
      ExploreStyles.MultiPanelTile,
      InviteUserPopup(props.programId, props.invitations, ref),
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
  }

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
      .useOverlayPanelRef
      .render: (props, ctx, resize, overlayRef) =>
        renderFn(props, resize, overlayRef)(using ctx)
