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
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.common.ProposalQueries.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.*
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.CallForProposal
import explore.model.CoIInvitation
import explore.model.ExploreGridLayouts
import explore.model.PartnerSplit
import explore.model.ProgramTimeRange
import explore.model.ProgramUserWithRole
import explore.model.Proposal
import explore.model.ProposalAttachment
import explore.model.ProposalTabTileIds
import explore.model.ProposalType
import explore.model.ProposalType.*
import explore.model.display.given
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.core.syntax.all.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.SelectItem
import lucuma.react.primereact.hooks.UseOverlayPanelRef.implicits.*
import lucuma.react.resizeDetector.UseResizeDetectorReturn
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.reusability.given
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.common.CallsQueriesGQL.*
import queries.common.ProposalQueriesGQL
import spire.std.any.*
import explore.Icons
import lucuma.react.primereact.Button
import explore.components.FormStaticData

case class ProposalEditor(
  programId:         Program.Id,
  optUserId:         Option[User.Id],
  proposal:          View[Proposal],
  undoStacks:        View[UndoStacks[IO, Proposal]],
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  users:             View[NonEmptyList[ProgramUserWithRole]],
  invitations:       View[List[CoIInvitation]],
  attachments:       View[List[ProposalAttachment]],
  authToken:         Option[NonEmptyString],
  layout:            LayoutsMap,
  readonly:          Boolean
) extends ReactFnProps(ProposalEditor.component)

object ProposalEditor:
  private type Props = ProposalEditor

  // private val Hours2Micros = BigDecimal(60L * 60L * 1000L * 1000L)

  // private def toHours(time: TimeSpan): Hours =
  //   Hours.from(time.toHours).getOrElse(Hours.Max)

  // private def fromHours(hours: Hours): TimeSpan =
  //   TimeSpan.unsafeFromMicroseconds((hours.value * Hours2Micros).longValue)

  // private def formatHours(hours: BigDecimal) = f"$hours%.2fh"

  private def sortedSplits(splits: List[PartnerSplit]): List[PartnerSplit] =
    splits
      .sortBy(_.percent.value)(Ordering[Int].reverse)

  private def partnerSplits(splits: List[PartnerSplit]): TagMod = splits match {
    case a if a.isEmpty =>
      <.span(
        Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
        " Partner time allocations are required."
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
            ^.alt := s"${partner.shortName}  Flag",
            ExploreStyles.PartnerSplitFlag
      )
    val span: TagMod = <.span(data)

    FormStaticData(id = id, value = <.div(img, span), label = partner.shortName)(
      ExploreStyles.FlexShrink(0.refined),
      ExploreStyles.PartnerSplitData
    )
  }

  // private def timeSplits(splits: SortedMap[Partner, IntPercent], total: TimeSpan): VdomNode =
  //   val tagmod = splits match {
  //     case a if a.isEmpty => TagMod.empty
  //     case _              =>
  //       sortedSplits(splits)
  //         .toTagMod(ps => timeSplit(ps, total))
  //   }
  //   <.div(tagmod, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  //
  // private def timeSplit(ps: PartnerSplit, total: TimeSpan) = {
  //   val splitTime = ps.percent.value * toHours(total).value / 100
  //   val timeText  = formatHours(splitTime)
  //   <.span(timeText, ExploreStyles.PartnerSplitData)
  // }

  // private def minimumTime(pct: IntPercent, total: TimeSpan) = {
  //   val time     = pct.value * toHours(total).value / 100
  //   val timeText = formatHours(time)
  //   <.div(<.span(timeText), ExploreStyles.PartnerSplitsGridMinPct)
  // }

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

  private def renderDetails(
    aligner:       Aligner[Proposal, ProposalPropertiesInput],
    undoCtx:       UndoContext[Proposal],
    // totalHours:        View[Hours],
    // minPct2:           View[IntPercent],
    // proposalClassType: View[ProposalClassType],
    showDialog:    View[PartnersDialogState],
    splitsList:    View[List[PartnerSplit]],
    // splitsMap:     SortedMap[Partner, IntPercent],
    // timeEstimateRange: Pot[Option[ProgramTimeRange]],
    cfps:          List[CallForProposal],
    readonly:      Boolean,
    renderInTitle: Tile.RenderInTitle
  )(using Logger[IO]): VdomNode = {
    val proposalCfpView: View[Proposal] =
      aligner.viewMod { p =>
        ProposalPropertiesInput.callId.replace(p.callId.orUnassign) >>>
          ProposalPropertiesInput.`type`.replace(p.proposalType.map(_.toInput).orUnassign)
      }

    val titleAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.title, ProposalPropertiesInput.title.modify)

    val titleView = titleAligner.view(_.orUnassign)

    val callId: Option[CallForProposals.Id] = aligner.get.callId
    val scienceSubtype                      = aligner.get.proposalType.map(_.scienceSubtype)

    val selectedCfp   = callId.flatMap(id => cfps.find(_.id === id))
    val isCfpSelected = selectedCfp.isDefined
    val subtypes      =
      selectedCfp.map(_.cfpType.subTypes)
    val hasSubtypes   =
      subtypes.exists(_.size > 1)

    val categoryAligner: Aligner[Option[TacCategory], Input[TacCategory]] =
      aligner.zoom(Proposal.category, ProposalPropertiesInput.category.modify)

    val categoryView: View[Option[TacCategory]] = categoryAligner.view(_.orUnassign)

    val proposalTypeAligner: Aligner[Option[ProposalType], Input[ProposalTypeInput]] =
      aligner.zoom(Proposal.proposalType, ProposalPropertiesInput.`type`.modify)

    val proposalTypeView: View[Option[ProposalType]] =
      proposalTypeAligner.view(_.map(_.toInput).orUnassign)

    val activationAligner: Option[Aligner[ToOActivation, Input[ToOActivation]]] =
      aligner
        .zoomOpt(Proposal.proposalType.some.andThen(ProposalType.toOActivation),
                 modifyToOActivation
        )
        .filter(_ => isCfpSelected)

    val activationView: Option[View[ToOActivation]] = activationAligner.map(_.view(_.assign))

    val needsPartnerSelection =
      scienceSubtype match {
        // Queue is set by default even if there is no CfP selection
        case Some(ScienceSubtype.Queue) | Some(ScienceSubtype.Classical) => isCfpSelected
        case _                                                           => false
      }

    val splitsAligner: Option[Aligner[List[PartnerSplit], Input[List[PartnerSplitInput]]]] =
      aligner
        .zoomOpt(Proposal.proposalType.some.andThen(ProposalType.partnerSplits),
                 modifyPartnerSplits
        )
        .filter(_ => needsPartnerSelection)

    val partnerSplitsView: Option[View[List[PartnerSplit]]] =
      splitsAligner.map(
        _.view(_.map(p => PartnerSplitInput(p.partner, p.percent)).assign)
      )

    // println(s"sub type; ${proposalTypeView.get.map(_.scienceSubtype)}")
    println(s"Selected call:: ${selectedCfp.map(_.id)}")
    // println(s"Selected partners:: ${selectedCfp.foldMap(_.partners)}")
    // println(needsPartnerSelection)
    // println(splitsView)
    // pprint.pprintln(aligner.get)
    pprint.pprintln(activationView.map(_.get))

    // val splitsList = splitsView.foldMap(_.get)

    // val totalTimeView   = classView.zoom(ProposalClass.totalTime)
    // val totalTime       = totalTimeView.get
    // val minimumPct1View = classView.zoom(ProposalClass.minPercentTime)
    // val minimumPct2View = classView.zoom(ProposalClass.minPercentTotalTime)

    // val minExecutionPot: Pot[TimeSpan] = timeEstimateRange.map(_.map(_.minimum.value).orEmpty)
    // val maxExecutionPot: Pot[TimeSpan] = timeEstimateRange.map(_.map(_.maximum.value).orEmpty)

    // val (maxTimeLabel, minTimeLabel) =
    //   ("Max Time", "Min Time")
    // ProposalClassType.fromProposalClass(classView.get) match {
    //   case LargeProgram | Intensive => ("Semester Max", "Semester Min")
    //   case _                        => ("Max Time", "Min Time")
    // }

    // def makeMinimumPctInput[A](pctView: View[IntPercent], id: NonEmptyString): TagMod =
    //   FormInputTextView(
    //     value = pctView,
    //     validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
    //     changeAuditor = ChangeAuditor.refinedInt[ZeroTo100](),
    //     label = React.Fragment("Minimum %", HelpIcon("proposal/main/minimum-pct.md".refined)),
    //     id = id,
    //     disabled = readonly,
    //     inputClass = ExploreStyles.PartnerSplitsGridMinPct
    //   )

    // def totalTimeEntry[A]: VdomNode =
    //   FormInputTextView(
    //     value = totalHours.withOnMod(h => totalTimeView.set(fromHours(h))),
    //     validFormat = ExploreModelValidators.hoursValidWedge,
    //     changeAuditor = ChangeAuditor.accept.decimal(2.refined),
    //     label = React.Fragment("Total", HelpIcon("proposal/main/total-time.md".refined)),
    //     id = "total-time-entry".refined,
    //     disabled = readonly,
    //     inputClass = ExploreStyles.PartnerSplitsGridTotal
    //   )

    // def openPartnerSplitsEditor: Callback = {
    //   val allPartners = Enumerated[Partner].all.map(p =>
    //     splitsMap
    //       .get(p)
    //       .fold(PartnerSplit(p, 0.refined))(pct => PartnerSplit(p, pct))
    //   )
    //   splitsList.set(allPartners) >> showDialog.set(true)
    // }

    React.Fragment(
      renderInTitle(<.div(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx))),
      <.form(
        <.div(ExploreStyles.ProposalDetailsGrid)(
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            // Title input
            FormInputTextView(
              id = "title".refined,
              inputClass = Css("inverse"),
              value = titleView,
              validFormat = InputValidSplitEpi.nonEmptyString.optional,
              label = "Title",
              disabled = readonly
            )(^.autoFocus := true),
            // Category selector
            FormDropdownOptional(
              id = "category".refined,
              label = React.Fragment("Category", HelpIcon("proposal/main/category.md".refined)),
              value = categoryView.get.map(categoryTag),
              options = categoryOptions,
              onChange = _.map(v => categoryView.set(Enumerated[TacCategory].fromTag(v))).orEmpty,
              disabled = readonly,
              modifiers = List(^.id := "category")
            ),
            partnerSplitsView.map(psView =>
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
                      onClick = showDialog.set(PartnersDialogState.Shown),
                      tooltip = "Edit Partner Splits",
                      disabled = readonly
                    ).mini.compact
                  )
                ),
                PartnerSplitsEditor(
                  showDialog.get,
                  splitsList,
                  showDialog.set(PartnersDialogState.Hidden),
                  splits => psView.set(splits)
                ),
                partnerSplits(psView.get)
              )
              // <.div(
              //   makeMinimumPctInput(minimumPct1View, "min-pct-1".refined)
              // ),
              // The second partner splits row, for maximum times - is always there
              // FormStaticData(
              //   value = maxExecutionPot.orSpinner(t => formatHours(toHours(t))),
              //   label = maxTimeLabel,
              //   id = "maxTime"
              // ),
              // maxExecutionPot.renderPot(
              //   valueRender = maxExecutionTime =>
              //     React.Fragment(timeSplits(splitsMap, maxExecutionTime),
              //                    minimumTime(minimumPct1View.get, maxExecutionTime)
              //     ),
              //   pendingRender = React.Fragment(<.span(), <.span())
              // ),
              // The third partner splits row, for minimum times - is always there
              // FormStaticData(
              //   value = minExecutionPot.orSpinner(t => formatHours(toHours(t))),
              //   label = minTimeLabel,
              //   id = "maxTime"
              // )
              // minExecutionPot.renderPot(
              //   valueRender = minExecutionTime =>
              //     React.Fragment(timeSplits(splitsMap, minExecutionTime),
              //                    minimumTime(minimumPct1View.get, minExecutionTime)
              //     ),
              //   pendingRender = React.Fragment(<.span(), <.span())
              // ),
              // // The third partner splits row - only exists for a few observation classes
              // totalTime.fold(React.Fragment()) { tt =>
              //   React.Fragment(
              //     <.div(totalTimeEntry),
              //     timeSplits(splitsMap, tt),
              //     <.div(
              //       minimumPct2View
              //         .mapValue(pctView => makeMinimumPctInput(pctView, "min-pct-2".refined))
              //         .getOrElse(minimumTime(minimumPct1View.get, tt))
              //     )
              //   )
              // }
            )
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            // Call for proposal selector
            FormDropdownOptional(
              id = "cfp".refined,
              label = React.Fragment("Call For Proposal", HelpIcon("proposal/main/cfp.md".refined)),
              value = callId,
              options = cfps.map(r => SelectItem(r.id, r.title)),
              onChange = _.map { cid =>
                val call = cfps.find(_.id === cid)
                proposalCfpView.mod(
                  _.copy(callId = cid.some, proposalType = call.map(_.cfpType.defaultType))
                )
              }.orEmpty,
              disabled = readonly,
              modifiers = List(^.id := "cfp")
            ),
            // Proposal type selector, visible when cfp is selected and has more than one subtpye
            FormDropdown(
              id = "proposalType".refined,
              options = subtypes.foldMap(_.toList).map(st => SelectItem(st, st.shortName)),
              label = React.Fragment("Regular Proposal Type",
                                     HelpIcon("proposal/main/proposal-type.md".refined)
              ),
              value = proposalTypeView.get.map(_.scienceSubtype).orNull,
              onChange = v => proposalTypeView.mod(_.map(ProposalType.toScienceSubtype(v))),
              disabled = readonly,
              modifiers = List(^.id := "proposalType")
            ).when(hasSubtypes),
            activationView.map(activationView =>
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
    )
  }

  private def renderFn(
    programId:    Program.Id,
    optUserId:    Option[User.Id],
    proposal:     View[Proposal],
    undoStacks:   View[UndoStacks[IO, Proposal]],
    // totalHours:        View[Hours],
    // minPct2:           View[IntPercent],
    showDialog:   View[PartnersDialogState],
    splitsList:   View[List[PartnerSplit]],
    createInvite: View[CreateInviteProcess],
    // timeEstimateRange: Pot[Option[ProgramTimeRange]],
    users:        View[NonEmptyList[ProgramUserWithRole]],
    invitations:  View[List[CoIInvitation]],
    attachments:  View[List[ProposalAttachment]],
    cfps:         List[CallForProposal],
    authToken:    Option[NonEmptyString],
    layout:       LayoutsMap,
    readonly:     Boolean,
    resize:       UseResizeDetectorReturn,
    ref:          OverlayPanelRef
  )(using ctx: AppContext[IO]) = {
    import ctx.given

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

    val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.abstrakt, ProposalPropertiesInput.`abstract`.modify)

    val abstractView = abstractAligner.view(_.orUnassign)

    val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

    val detailsTile =
      Tile(ProposalTabTileIds.DetailsId.id, "Details", canMinimize = true)(
        renderDetails(
          aligner,
          undoCtx,
          // totalHours,
          // minPct2,
          showDialog,
          splitsList,
          // splitsView.get,
          // timeEstimateRange,
          cfps,
          readonly,
          _
        )
      )

    val usersTile = ProgramUsers.programUsersTile(programId, users, invitations, createInvite, ref)

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
      )
    ).withRef(resize.ref)
  }
  summon[Reusability[CallForProposals.Id]]

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
      // .useStateViewBy: (props, _, _) =>
      //   // total time - we need `Hours` for editing and also to preserve if
      //   // the user switches between classes with and without total time.
      //   props.proposal
      //     .zoom(Proposal.proposalClass.andThen(ProposalClass.totalTime))
      //     .get
      //     .map(toHours)
      //     .getOrElse(Hours.unsafeFrom(0))
      // .useStateViewBy((props, _, _, _) =>
      //   // mininum percent total time = need to preserve between class switches
      //   props.proposal
      //     .zoom(Proposal.proposalClass.andThen(ProposalClass.minPercentTotalTime))
      //     .get
      //     .getOrElse(IntPercent.unsafeFrom(80))
      // )
      // .useStateViewBy((props, _, _, _, _) =>
      //   // Initial proposal class type
      //   ProposalClassType.fromProposalClass(props.proposal.get.proposalClass)
      // )
      .useStateView(PartnersDialogState.Hidden) // show partner splits modal
      .useStateView(List.empty[PartnerSplit]) // show partner splits modal
      // Update the partner splits when a new callId is set
      .useEffectWithDepsBy((props, _, cfps, _, _) =>
        (props.proposal.get.callId, cfps.toOption.orEmpty)
      ) { (props, _, _, _, ps) => (callId, cfps) =>
        // val m: Int = cfps.foldMap(_.find(_.id === proposal.get.callId))
        callId.fold(Callback.empty)(cid =>
          val currentSplits = Proposal.proposalType.some
            .andThen(ProposalType.partnerSplits)
            .getOption(props.proposal.get)
          val cfpPartners   = cfps
            .find(_.id === cid)
            .foldMap(_.partners.map(_.partner))
          ps.set(cfpPartners.map(p => PartnerSplit(p, 0.refined)))
            .when_(currentSplits.foldMap(_.map(_.partner)) =!= cfpPartners)
        )
      }
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
      .useStateView(CreateInviteProcess.Idle)
      .useOverlayPanelRef
      .render {
        (
          props,
          ctx,
          cfps,
          // totalHours,
          // minPct2,
          showDialog,
          splitsList,
          resize,
          createInvite,
          overlayRef
        ) =>
          renderFn(
            props.programId,
            props.optUserId,
            props.proposal,
            props.undoStacks,
            // totalHours,
            // minPct2,
            showDialog,
            splitsList,
            createInvite,
            // props.timeEstimateRange,
            props.users,
            props.invitations,
            props.attachments,
            cfps.toOption.orEmpty,
            props.authToken,
            props.layout,
            props.readonly,
            resize,
            overlayRef
          )(using ctx)
      }
