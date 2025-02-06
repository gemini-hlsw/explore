// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import clue.*
import clue.data.Input
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.common.ProgramQueries
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.*
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.CallForProposal
import explore.model.Constants
import explore.model.ExploreGridLayouts
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalTabTileIds
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.*
import explore.users.AddReadonlyCoiButton
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.common.ProposalQueriesGQL

case class ProposalEditor(
  programId:          Program.Id,
  userVault:          Option[UserVault],
  undoCtx:            UndoContext[ProgramDetails],
  proposal:           UndoSetter[Proposal],
  timeEstimateRange:  Pot[Option[ProgramTimeRange]],
  users:              View[List[ProgramUser]],
  attachments:        View[AttachmentList],
  authToken:          Option[NonEmptyString],
  cfps:               List[CallForProposal],
  layout:             LayoutsMap,
  proposalIsReadonly: Boolean,
  userIsReadonlyCoi:  Boolean
) extends ReactFnProps(ProposalEditor.Component):
  val optUserId: Option[User.Id]        = userVault.map(_.user.id)
  val proposalOrUserIsReadonly: Boolean = proposalIsReadonly || userIsReadonlyCoi

object ProposalEditor:

  private val BaseWordLimit = 200
  private val HardWordLimit = 2 * BaseWordLimit

  extension (s: String)
    inline def wordCount: Int =
      val trim = s.trim
      if (trim.isEmpty) 0
      else
        trim.split("\\s+", HardWordLimit + 1).length // add a limit to restrict the performance hit

  private val Component = ScalaFnComponent[ProposalEditor](props =>
    for {
      ctx             <- useContext(AppContext.ctx)
      abstractCounter <-
        useState(props.undoCtx.get.description.map(_.value).foldMap(_.wordCount))
      _               <- useEffectWithDeps(props.undoCtx.get.description.map(_.value)) {
                           case Some(t) => abstractCounter.setState(t.wordCount)
                           case None    => abstractCounter.setState(0)
                         }
      resize          <- useResizeDetector
    } yield
      import ctx.given

      val detailsAligner: Aligner[ProgramDetails, ProgramPropertiesInput] =
        Aligner(
          props.undoCtx,
          UpdateProgramsInput(
            WHERE = props.programId.toWhereProgram.assign,
            SET = ProgramPropertiesInput()
          ),
          ProgramQueries.updateProgram[IO](_)
        ).zoom(Iso.id[ProgramDetails].asLens, UpdateProgramsInput.SET.modify)

      val proposalAligner: Aligner[Proposal, ProposalPropertiesInput] =
        Aligner(
          props.proposal,
          UpdateProposalInput(
            programId = props.programId.assign,
            SET = ProposalPropertiesInput()
          ),
          (ProposalQueriesGQL.UpdateProposalMutation[IO].execute(_)).andThen(_.void)
        ).zoom(Iso.id[Proposal].asLens, UpdateProposalInput.SET.modify)

      val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
        detailsAligner.zoom(ProgramDetails.description, ProgramPropertiesInput.description.modify)

      val abstractView: View[Option[NonEmptyString]] = abstractAligner
        .view(_.orUnassign)

      val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

      val detailsTile =
        Tile(ProposalTabTileIds.DetailsId.id, "Details")(
          _ =>
            ProposalDetailsBody(
              detailsAligner,
              proposalAligner,
              props.timeEstimateRange,
              props.cfps,
              props.proposalOrUserIsReadonly
            ),
          (_, s) => ProposalDetailsTitle(props.undoCtx, s, props.proposalOrUserIsReadonly)
        )

      val usersTile =
        Tile(
          ProposalTabTileIds.UsersId.id,
          "Investigators"
        )(
          _ =>
            ProgramUsersTable(
              props.userVault,
              props.users,
              NonEmptySet.of(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.CoiRO),
              props.proposalIsReadonly,
              props.userIsReadonlyCoi
            ),
          (_, _) =>
            <.div(
              ExploreStyles.AddProgramUserButton,
              Option
                .unless[VdomNode](props.proposalOrUserIsReadonly):
                  AddReadonlyCoiButton(props.programId, props.users)
                .orEmpty,
              HelpIcon("proposal/main/investigators.md".refined)
            )
        )

      val absTitle: VdomNode =
        if (abstractCounter.value < 1) "Abstract"
        else if (abstractCounter.value >= HardWordLimit)
          React.Fragment(
            "Abstract ",
            <.span(ExploreStyles.AbstractTitleTooLong, s"($HardWordLimit or more words)")
          )
        else if (abstractCounter.value >= BaseWordLimit)
          React.Fragment(
            "Abstract ",
            <.span(ExploreStyles.AbstractTitleTooLong, s"(${abstractCounter.value} words)")
          )
        else s"Abstract (${abstractCounter.value} words)"

      val abstractTile =
        Tile(
          ProposalTabTileIds.AbstractId.id,
          absTitle,
          bodyClass = ExploreStyles.ProposalAbstract
        )(_ =>
          FormInputTextAreaView(
            id = "abstract".refined,
            value = abstractView.as(OptionNonEmptyStringIso),
            onTextChange = t => abstractCounter.setState(t.wordCount).rateLimitMs(1000).void
          )(^.disabled        := props.proposalOrUserIsReadonly,
            ^.cls := ExploreStyles.WarningInput.when_(abstractView.get.isEmpty).htmlClass
          )
        )

      val attachmentsTile =
        Tile(
          ProposalTabTileIds.AttachmentsId.id,
          "Attachments",
          tileClass = ExploreStyles.ProposalAttachmentsTile
        )(
          _ =>
            props.authToken.map(token =>
              ProposalAttachmentsTable(
                props.programId,
                token,
                props.attachments,
                props.proposalOrUserIsReadonly
              )
            ),
          (_, _) =>
            <.a(^.href           := Constants.P1TemplatesUrl,
                ^.target := "_blank",
                Icons.ArrowUpRightFromSquare
            ).withTooltip("Download templates")
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
  )
