// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order.*
import cats.data.Chain
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.*
import clue.data.Input
import clue.data.syntax.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.FormStaticData
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.*
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.CallForProposal
import explore.model.CoIInvitation
import explore.model.ExploreGridLayouts
import explore.model.ExploreModelValidators
import explore.model.Hours
import explore.model.ProgramTimeRange
import explore.model.ProgramUserWithRole
import explore.model.ProposalAttachment
import explore.model.ProposalTabTileIds
import explore.model.display.given
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.proposal.ProposalClassType.*
import explore.syntax.ui.*
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.ProposalClass
import lucuma.core.model.User
import lucuma.core.model.ZeroTo100
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.SelectItem
import lucuma.react.primereact.hooks.UseOverlayPanelRef.implicits.*
import lucuma.react.resizeDetector.UseResizeDetectorReturn
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.common.CallsQueriesGQL.*
import queries.common.ProposalQueriesGQL
import spire.std.any.*

import scala.collection.immutable.SortedMap

case class ProposalEditor(
  programId:         Program.Id,
  optUserId:         Option[User.Id],
  proposal:          View[Proposal],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  users:             View[NonEmptyList[ProgramUserWithRole]],
  invitations:       List[CoIInvitation],
  attachments:       View[List[ProposalAttachment]],
  authToken:         Option[NonEmptyString],
  layout:            LayoutsMap,
  readonly:          Boolean
) extends ReactFnProps(ProposalEditor.component)

object ProposalEditor:
  private type Props = ProposalEditor

  private val Hours2Micros = BigDecimal(60L * 60L * 1000L * 1000L)

  private def toHours(time: TimeSpan): Hours =
    Hours.from(time.toHours).getOrElse(Hours.Max)

  private def fromHours(hours: Hours): TimeSpan =
    TimeSpan.unsafeFromMicroseconds((hours.value * Hours2Micros).longValue)

  private def formatHours(hours: BigDecimal) = f"$hours%.2fh"

  private def sortedSplits(splits: SortedMap[Partner, IntPercent]): List[PartnerSplit] =
    splits.toList
      .map { case (part, perc) => PartnerSplit(part, perc) }
      .sortBy(_.percent.value)(Ordering[Int].reverse)

  private def partnerSplits(splits: SortedMap[Partner, IntPercent]): TagMod = splits match {
    case a if a.isEmpty =>
      <.span(
        Icons.ExclamationTriangle,
        "Partner time allocations are required."
      )
    case _              =>
      val ps = sortedSplits(splits)
        .toTagMod(ps => partnerSplit(ps))
      <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  }

  private def partnerSplit(ps: PartnerSplit): TagMod = {
    val id   = s"${ps.partner.tag}-split"
    val text = f"${ps.percent.value}%%"
    partnerSplitData(ps.partner, id, text)
  }

  private def partnerSplitData(partner: Partner, id: String, data: String) = {
    val img: TagMod  =
      <.img(^.src        := PartnerFlags.smallFlag(partner),
            ^.alt := s"${partner.name}  Flag",
            ExploreStyles.PartnerSplitFlag
      )
    val span: TagMod = <.span(data)

    FormStaticData(id = id, value = <.div(img, span), label = partner.shortName)(
      ExploreStyles.FlexShrink(0.refined),
      ExploreStyles.PartnerSplitData
    )
  }

  private def timeSplits(splits: SortedMap[Partner, IntPercent], total: TimeSpan): VdomNode =
    val tagmod = splits match {
      case a if a.isEmpty => TagMod.empty
      case _              =>
        sortedSplits(splits)
          .toTagMod(ps => timeSplit(ps, total))
    }
    <.div(tagmod, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)

  private def timeSplit(ps: PartnerSplit, total: TimeSpan) = {
    val splitTime = ps.percent.value * toHours(total).value / 100
    val timeText  = formatHours(splitTime)
    <.span(timeText, ExploreStyles.PartnerSplitData)
  }

  private def minimumTime(pct: IntPercent, total: TimeSpan) = {
    val time     = pct.value * toHours(total).value / 100
    val timeText = formatHours(time)
    <.div(<.span(timeText), ExploreStyles.PartnerSplitsGridMinPct)
  }

  private def categoryTag(category: TacCategory): String = Enumerated[TacCategory].tag(category)

  private val categoryOptions: List[SelectItem[String]] =
    Enumerated[TacCategory].all
      .groupBy(_.group)
      .toList
      .sortBy(_._1)
      .foldRight(Chain.empty[SelectItem[String]]) { case ((group, cats), acc) =>
        val groupItem =
          SelectItem(
            label = group.label,
            value = group.label,
            className = "header",
            disabled = true
          )
        val catItems  =
          Chain.fromSeq(cats.map(cat => SelectItem(label = cat.label, value = categoryTag(cat))))
        groupItem +: (catItems ++ acc)
      }
      .toList

  private def saveStateSplits(
    splitView: View[SortedMap[Partner, IntPercent]],
    splitList: List[PartnerSplit]
  ): Callback =
    splitView
      .set(SortedMap.from(splitList.filter(_.percent.value > 0).map(_.toTuple)))

  private def renderDetails(
    aligner:           Aligner[Proposal, ProposalPropertiesInput],
    undoCtx:           UndoContext[Proposal],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showDialog:        View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    splitsMap:         SortedMap[Partner, IntPercent],
    timeEstimateRange: Pot[Option[ProgramTimeRange]],
    cfps:              List[CallForProposal],
    selectedCfp:       View[Option[CallForProposal]],
    readonly:          Boolean,
    renderInTitle:     Tile.RenderInTitle
  )(using Logger[IO]): VdomNode = {
    val titleAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.title, ProposalPropertiesInput.title.modify)

    val titleView = titleAligner.view(_.orUnassign)

    val classAligner: Aligner[ProposalClass, Input[ProposalClassInput]] =
      aligner.zoom(Proposal.proposalClass, ProposalPropertiesInput.proposalClass.modify)

    val classView: View[ProposalClass] = classAligner.view(_.toInput.assign)

    val categoryAligner: Aligner[Option[TacCategory], Input[TacCategory]] =
      aligner.zoom(Proposal.category, ProposalPropertiesInput.category.modify)

    val categoryView: View[Option[TacCategory]] = categoryAligner.view(_.orUnassign)

    val activationAligner: Aligner[ToOActivation, Input[ToOActivation]] =
      aligner.zoom(Proposal.toOActivation, ProposalPropertiesInput.toOActivation.modify)

    val activationView: View[ToOActivation] = activationAligner.view(_.assign)

    val totalTimeView   = classView.zoom(ProposalClass.totalTime)
    val totalTime       = totalTimeView.get
    val minimumPct1View = classView.zoom(ProposalClass.minPercentTime)
    val minimumPct2View = classView.zoom(ProposalClass.minPercentTotalTime)

    val minExecutionPot: Pot[TimeSpan] = timeEstimateRange.map(_.map(_.minimum.value).orEmpty)
    val maxExecutionPot: Pot[TimeSpan] = timeEstimateRange.map(_.map(_.maximum.value).orEmpty)

    val (maxTimeLabel, minTimeLabel) =
      ProposalClassType.fromProposalClass(classView.get) match {
        case LargeProgram | Intensive => ("Semester Max", "Semester Min")
        case _                        => ("Max Time", "Min Time")
      }

    def makeMinimumPctInput[A](pctView: View[IntPercent], id: NonEmptyString): TagMod =
      FormInputTextView(
        value = pctView,
        validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
        changeAuditor = ChangeAuditor.refinedInt[ZeroTo100](),
        label = React.Fragment("Minimum %", HelpIcon("proposal/main/minimum-pct.md".refined)),
        id = id,
        disabled = readonly,
        inputClass = ExploreStyles.PartnerSplitsGridMinPct
      )

    def totalTimeEntry[A]: VdomNode =
      FormInputTextView(
        value = totalHours.withOnMod(h => totalTimeView.set(fromHours(h))),
        validFormat = ExploreModelValidators.hoursValidWedge,
        changeAuditor = ChangeAuditor.accept.decimal(2.refined),
        label = React.Fragment("Total", HelpIcon("proposal/main/total-time.md".refined)),
        id = "total-time-entry".refined,
        disabled = readonly,
        inputClass = ExploreStyles.PartnerSplitsGridTotal
      )

    def openPartnerSplitsEditor: Callback = {
      val allPartners = Partner.EnumeratedPartner.all.map(p =>
        splitsMap
          .get(p)
          .fold(PartnerSplit(p, 0.refined))(pct => PartnerSplit(p, pct))
      )
      splitsList.set(allPartners) >> showDialog.set(true)
    }

    def onClassTypeMod(classType: ProposalClassType): Callback = {
      val currentClass    = classView.get
      val minPctTime      = currentClass.minPercentTime
      val minPctTotalTime =
        ProposalClass.minPercentTotalTime.getOption(currentClass).getOrElse(minPct2.get)
      val totalTime       =
        ProposalClass.totalTime.getOption(currentClass).getOrElse(fromHours(totalHours.get))
      val newClass        = classType.toProposalClass(minPctTime, minPctTotalTime, totalTime)
      classView.set(newClass) >> minPct2.set(minPctTotalTime) >> totalHours.set(toHours(totalTime))
    }

    React.Fragment(
      renderInTitle(<.div(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx))),
      <.form(
        <.div(ExploreStyles.ProposalDetailsGrid)(
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            FormInputTextView(
              id = "title".refined,
              inputClass = Css("inverse"),
              value = titleView,
              validFormat = InputValidSplitEpi.nonEmptyString.optional,
              label = "Title",
              disabled = readonly
            )(^.autoFocus := true),
            <.div(
              ExploreStyles.PartnerSplitsGrid,
              // The first partner splits row, with the button and the flags
              <.div(
                <.label("Partners"),
                <.div(
                  Button(
                    icon = Icons.Edit,
                    severity = Button.Severity.Secondary,
                    tpe = Button.Type.Button,
                    onClick = openPartnerSplitsEditor,
                    tooltip = "Edit Partner Splits",
                    disabled = readonly
                  ).mini.compact
                )
              ),
              partnerSplits(splitsMap),
              <.div(
                makeMinimumPctInput(minimumPct1View, "min-pct-1".refined)
              ),
              // The second partner splits row, for maximum times - is always there
              FormStaticData(
                value = maxExecutionPot.orSpinner(t => formatHours(toHours(t))),
                label = maxTimeLabel,
                id = "maxTime"
              ),
              maxExecutionPot.renderPot(
                valueRender = maxExecutionTime =>
                  React.Fragment(timeSplits(splitsMap, maxExecutionTime),
                                 minimumTime(minimumPct1View.get, maxExecutionTime)
                  ),
                pendingRender = React.Fragment(<.span(), <.span())
              ),
              // The third partner splits row, for minimum times - is always there
              FormStaticData(
                value = minExecutionPot.orSpinner(t => formatHours(toHours(t))),
                label = minTimeLabel,
                id = "maxTime"
              ),
              minExecutionPot.renderPot(
                valueRender = minExecutionTime =>
                  React.Fragment(timeSplits(splitsMap, minExecutionTime),
                                 minimumTime(minimumPct1View.get, minExecutionTime)
                  ),
                pendingRender = React.Fragment(<.span(), <.span())
              ),
              // The third partner splits row - only exists for a few observation classes
              totalTime.fold(React.Fragment()) { tt =>
                React.Fragment(
                  <.div(totalTimeEntry),
                  timeSplits(splitsMap, tt),
                  <.div(
                    minimumPct2View
                      .mapValue(pctView => makeMinimumPctInput(pctView, "min-pct-2".refined))
                      .getOrElse(minimumTime(minimumPct1View.get, tt))
                  )
                )
              }
            )
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            <.div(
              LucumaPrimeStyles.FormField,
              ExploreStyles.ProposalCfpFields,
              FormDropdownOptional(
                id = "cfp".refined,
                label =
                  React.Fragment("Call For Proposal", HelpIcon("proposal/main/cfp.md".refined)),
                value = selectedCfp.get,
                options = cfps.map(r => SelectItem(r, r.semester.format)),
                onChange = _.map(v => selectedCfp.set(v.some)).orEmpty,
                disabled = readonly,
                modifiers = List(^.id := "cfp")
              ),
              FormEnumDropdownView(
                id = "proposal-class".refined,
                value = proposalClassType.withOnMod(onClassTypeMod),
                label = React.Fragment("Class", HelpIcon("proposal/main/class.md".refined)),
                disabled = readonly
              )
            ),
            FormDropdownOptional(
              id = "category".refined,
              label = React.Fragment("Category", HelpIcon("proposal/main/category.md".refined)),
              value = categoryView.get.map(categoryTag),
              options = categoryOptions,
              onChange = _.map(v => categoryView.set(Enumerated[TacCategory].fromTag(v))).orEmpty,
              disabled = readonly,
              modifiers = List(^.id := "category")
            ),
            FormEnumDropdownView(
              id = "too-activation".refined,
              value = activationView,
              label = React.Fragment(
                "ToO Activation",
                HelpIcon("proposal/main/too-activation.md".refined)
              ),
              disabled = readonly
            )
          )
        )
      )
    )
  }

  private def renderFn(
    programId:         Program.Id,
    optUserId:         Option[User.Id],
    proposal:          View[Proposal],
    undoStacks:        View[UndoStacks[IO, Proposal]],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showDialog:        View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    createInvite:      View[CreateInviteProcess],
    timeEstimateRange: Pot[Option[ProgramTimeRange]],
    users:             View[NonEmptyList[ProgramUserWithRole]],
    invitations:       View[List[CoIInvitation]],
    attachments:       View[List[ProposalAttachment]],
    cfps:              List[CallForProposal],
    selectedCfp:       View[Option[CallForProposal]],
    authToken:         Option[NonEmptyString],
    layout:            LayoutsMap,
    readonly:          Boolean,
    resize:            UseResizeDetectorReturn,
    ref:               OverlayPanelRef
  )(using ctx: AppContext[IO]) = {
    import ctx.given

    def closePartnerSplitsEditor: Callback = showDialog.set(false)

    val undoCtx: UndoContext[Proposal]                      = UndoContext(undoStacks, proposal)
    val aligner: Aligner[Proposal, ProposalPropertiesInput] =
      Aligner(
        undoCtx,
        UpdateProposalInput(
          programId = programId.assign,
          SET = ProposalPropertiesInput()
        ),
        (ProposalQueriesGQL.UpdateProposalMutation[IO].execute(_)).andThen(_.void)
      ).zoom(Iso.id[Proposal].asLens, UpdateProposalInput.SET.modify)

    val splitsAligner: Aligner[SortedMap[Partner, IntPercent], Input[List[PartnerSplitInput]]] =
      aligner.zoom(Proposal.partnerSplits, ProposalPropertiesInput.partnerSplits.modify)

    val splitsView: View[SortedMap[Partner, IntPercent]] =
      splitsAligner.view(
        _.toList
          .map { case (par, pct) =>
            PartnerSplitInput(partner = par, percent = pct)
          }
          .assign
      )

    val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

    val detailsTile =
      Tile(ProposalTabTileIds.DetailsId.id, "Details", canMinimize = true)(
        renderDetails(
          aligner,
          undoCtx,
          totalHours,
          minPct2,
          proposalClassType,
          showDialog,
          splitsList,
          splitsView.get,
          timeEstimateRange,
          cfps,
          selectedCfp,
          readonly,
          _
        )
      )

    val usersTile = ProgramUsers.programUsersTile(programId, users, invitations, createInvite, ref)

    val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.abstrakt, ProposalPropertiesInput.`abstract`.modify)

    val abstractView = abstractAligner.view(_.orUnassign)
    val abstractTile =
      Tile(ProposalTabTileIds.AbstractId.id,
           "Abstract",
           canMinimize = true,
           bodyClass = ExploreStyles.ProposalAbstract
      )(_ =>
        FormInputTextAreaView(
          id = "abstract".refined,
          value = abstractView.as(OptionNonEmptyStringIso)
        )(^.disabled := readonly)
      )

    val attachmentsTile =
      Tile(ProposalTabTileIds.AttachmentsId.id, "Attachments", canMinimize = true)(_ =>
        authToken.map(token => ProposalAttachmentsTable(programId, token, attachments, readonly))
      )

    <.div(
      ExploreStyles.MultiPanelTile,
      TileController(
        optUserId,
        resize.width.getOrElse(1),
        defaultLayouts,
        layout,
        List(detailsTile, usersTile, abstractTile, attachmentsTile),
        GridLayoutSection.ProposalLayout,
        storeLayout = true
      ),
      PartnerSplitsEditor(
        showDialog.get,
        splitsList,
        closePartnerSplitsEditor,
        saveStateSplits(splitsView, _)
      )
    ).withRef(resize.ref)
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cfps
      .useEffectResultOnMountBy: (_, ctx) =>
        import ctx.given
        ReadOpenCFPs[IO]
          .query()
          .map(_.map(_.callsForProposals.matches)) // .map(_: CallForProposal))
      .useStateViewBy((props, _, _) =>
        // total time - we need `Hours` for editing and also to preserve if
        // the user switches between classes with and without total time.
        props.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.totalTime))
          .get
          .map(toHours)
          .getOrElse(Hours.unsafeFrom(0))
      )
      .useStateViewBy((props, _, _, _) =>
        // mininum percent total time = need to preserve between class switches
        props.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.minPercentTotalTime))
          .get
          .getOrElse(IntPercent.unsafeFrom(80))
      )
      .useStateViewBy((props, _, _, _, _) =>
        // Initial proposal class type
        ProposalClassType.fromProposalClass(props.proposal.get.proposalClass)
      )
      .useStateView(false) // show partner splits modal
      .useStateView(List.empty[PartnerSplit])
      .useStateViewBy((props, _, _, _, _, _, _, _) => props.proposal.get.proposalClass)
      .useEffectWithDepsBy((props, _, _, _, _, _, _, _, _) => props.proposal.get.proposalClass)(
        // Deal with changes to the ProposalClass.
        (props, _, _, totalHours, minPct2, classType, _, _, oldClass) =>
          newClass => {
            val setClass =
              if (oldClass.get === newClass) Callback.empty
              else props.proposal.zoom(Proposal.proposalClass).set(newClass)
            val newType  = ProposalClassType.fromProposalClass(newClass)
            val setType  = if (classType.get === newType) Callback.empty else classType.set(newType)
            val newHours = ProposalClass.totalTime.getOption(newClass).map(toHours)
            val setHours = newHours
              .flatMap(h => if (h === totalHours.get) none else h.some)
              .foldMap(totalHours.set)
            val newPct2  = ProposalClass.minPercentTotalTime.getOption(newClass)
            val setPct2  =
              newPct2.flatMap(p => if (p === minPct2.get) none else p.some).foldMap(minPct2.set)
            setClass >> setType >> setHours >> setPct2
          }
      )
      .useResizeDetector()
      .useStateView(CreateInviteProcess.Idle)
      .useOverlayPanelRef
      .useStateViewBy((props, _, _, _, _, _, _, _, _, _, _, _) => props.invitations)
      .useStateView(none[CallForProposal])
      .render {
        (
          props,
          ctx,
          cfps,
          totalHours,
          minPct2,
          proposalClassType,
          showDialog,
          splitsList,
          _,
          resize,
          createInvite,
          overlayRef,
          invitations,
          selectedCfp
        ) =>
          renderFn(
            props.programId,
            props.optUserId,
            props.proposal,
            props.undoStacks,
            totalHours,
            minPct2,
            proposalClassType,
            showDialog,
            splitsList,
            createInvite,
            props.timeEstimateRange,
            props.users,
            invitations,
            props.attachments,
            cfps.toOption.orEmpty,
            selectedCfp,
            props.authToken,
            props.layout,
            props.readonly,
            resize,
            overlayRef
          )(using ctx)
      }
