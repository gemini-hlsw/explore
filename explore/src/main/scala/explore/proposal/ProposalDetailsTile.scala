// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order.*
import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import clue.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.common.ProposalOdbExtensions.*
import explore.components.FormStaticData
import explore.components.HelpIcon
import explore.components.ui.*
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.CallForProposal
import explore.model.ExploreModelValidators
import explore.model.Hours
import explore.model.PartnerSplit
import explore.model.ProgramDetails
import explore.model.ProgramTimeRange
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalType
import explore.model.ProposalType.*
import explore.model.display.given
import explore.model.enums.TileSizeState
import explore.model.enums.Visible
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.model.ZeroTo100
import lucuma.core.syntax.all.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.format.*
import lucuma.ui.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import spire.std.any.*

import scalajs.js.JSConverters.*

case class ProposalDetailsBody(
  detailsAligner:  Aligner[ProgramDetails, ProgramPropertiesInput],
  proposalAligner: Aligner[Proposal, ProposalPropertiesInput],
  cfps:            List[CallForProposal],
  users:           List[ProgramUser],
  setReviewer:     Option[ProgramUser] => Callback,
  setMentor:       Option[ProgramUser] => Callback,
  readonly:        Boolean
) extends ReactFnProps(ProposalDetailsBody.component):
  val proposal: Proposal                                           = proposalAligner.get
  val timeEstimateRange: CalculatedValue[Option[ProgramTimeRange]] =
    detailsAligner.get.programTimes.timeEstimateRange
  val pi: Option[ProgramUser]                                      = detailsAligner.get.pi

object ProposalDetailsBody:
  private type Props = ProposalDetailsBody

  private val Hours2Micros = BigDecimal(60L * 60L * 1000L * 1000L)

  private def toHours(time: TimeSpan): Hours =
    Hours.from(time.toHours).getOrElse(Hours.Max)

  private def fromHours(hours: Hours): TimeSpan =
    TimeSpan.unsafeFromMicroseconds((hours.value * Hours2Micros).longValue)

  private def formatHours(hours: BigDecimal) = f"$hours%.2fh"

  private def sortedSplits(splits: List[PartnerSplit]): List[PartnerSplit] =
    splits
      .filter(_.percent.value > 0)
      .sortBy(_.percent.value)(using Ordering[Int].reverse)

  private def partnerSplits(splits: List[PartnerSplit]): VdomNode =
    splits match
      case a if a.isEmpty =>
        <.div(
          ExploreStyles.PartnerSplitsMissing,
          Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
          " Partner time allocations are required."
        )
      case _              =>
        val ps = sortedSplits(splits).toTagMod(using ps => partnerSplit(ps))
        <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)

  private def partnerSplit(ps: PartnerSplit): TagMod =
    val id   = s"${ps.partner.tag}-split"
    val text = f"${ps.percent.value}%%"
    partnerSplitData(ps.partner, id, text)

  private def partnerSplitData(partner: Partner, id: String, data: String) =
    FormStaticData(
      id = id,
      value = <.div(partner.renderFlag, <.span(data)),
      label = partner.shortName
    )(
      ExploreStyles.FlexShrink(0.refined),
      ExploreStyles.PartnerSplitData
    )

  private def timeSplit(ps: PartnerSplit, total: CalculatedValue[TimeSpan]) =
    val splitTime = ps.percent.value * toHours(total.value).value / 100
    val timeText  = formatHours(splitTime)
    <.span(timeText, ExploreStyles.PartnerSplitData, total.staleClass).withOptionalTooltip(
      total.staleTooltip
    )

  private def minimumTime(pct: IntPercent, total: CalculatedValue[TimeSpan]) =
    val time     = pct.value * toHours(total.value).value / 100
    val timeText = formatHours(time)
    <.div(
      ExploreStyles.PartnerSplitsGridMinPctItem,
      ExploreStyles.PartnerSplitsGridMinPct,
      <.span(timeText, total.staleClass).withOptionalTooltip(total.staleTooltip)
    )

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

  private def renderFn(
    props:      Props,
    totalHours: View[Hours],
    showDialog: View[Visible],
    splitsList: View[List[PartnerSplit]]
  )(using Logger[IO]): VdomNode = {
    val proposalCfpView: View[Proposal] =
      props.proposalAligner.viewMod { p =>
        ProposalPropertiesInput.callId.replace(p.call.map(_.id).orUnassign) >>>
          ProposalPropertiesInput.`type`.replace(p.proposalType.map(_.toInput).orUnassign)
      }

    val titleAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      props.detailsAligner.zoom(ProgramDetails.name, ProgramPropertiesInput.name.modify)

    val titleView = titleAligner.view(_.orUnassign)

    val scienceSubtype = props.proposalAligner.get.proposalType.map(_.scienceSubtype)

    val selectedCfp: Option[CallForProposal] = props.proposalAligner.get.call
    val isCfpSelected                        = selectedCfp.isDefined
    val subtypes                             = selectedCfp.map(_.cfpType.subTypes)
    val hasSubtypes                          = subtypes.exists(_.size > 1)

    // need to include the current cfp as disabled if it doesn't exist in the list of calls
    val cfpOptions: List[SelectItem[CallForProposal]] =
      selectedCfp
        .filterNot(cfp => props.cfps.exists(_.id === cfp.id))
        .map(cfp => SelectItem(cfp, cfp.title, disabled = true))
        .toList ++
        props.cfps.map(cfp => SelectItem(cfp, cfp.title))

    val categoryAligner: Aligner[Option[TacCategory], Input[TacCategory]] =
      props.proposalAligner.zoom(Proposal.category, ProposalPropertiesInput.category.modify)

    val categoryView: View[Option[TacCategory]] = categoryAligner.view(_.orUnassign)

    val proposalTypeAligner: Aligner[Option[ProposalType], Input[ProposalTypeInput]] =
      props.proposalAligner.zoom(Proposal.proposalType, ProposalPropertiesInput.`type`.modify)

    val proposalTypeView: View[Option[ProposalType]] =
      proposalTypeAligner.view(_.map(_.toInput).orUnassign)

    val activationView: Option[View[ToOActivation]] =
      proposalTypeView.toOptionView.map(_.zoom(ProposalType.toOActivation).toOptionView).flatten

    val needsPartnerSelection =
      scienceSubtype match {
        // Queue is set by default even if there is no CfP selection
        case Some(ScienceSubtype.Queue) | Some(ScienceSubtype.Classical) => isCfpSelected
        case _                                                           => false
      }

    val partnerSplitsView: Option[View[List[PartnerSplit]]] =
      proposalTypeView.toOptionView
        .map(_.zoom(ProposalType.partnerSplits).toOptionView)
        .flatten
        .filter(_ => needsPartnerSelection)

    val minimumPct1View: Option[View[IntPercent]] =
      proposalTypeView.toOptionView.map(_.zoom(ProposalType.minPercentTime).toOptionView).flatten

    val minimumPct2View: Option[View[IntPercent]] =
      proposalTypeView.toOptionView
        .map(_.zoom(ProposalType.minPercentTotalTime).toOptionView)
        .flatten

    val totalTimeView: Option[View[TimeSpan]] =
      proposalTypeView.toOptionView.map(_.zoom(ProposalType.totalTime).toOptionView).flatten

    val minExecution: CalculatedValue[TimeSpan] =
      props.timeEstimateRange.map(_.map(_.minimum.value).orEmpty)
    val maxExecution: CalculatedValue[TimeSpan] =
      props.timeEstimateRange.map(_.map(_.maximum.value).orEmpty)

    val areTimesSame = minExecution.value === maxExecution.value

    val (maxTimeLabel, minTimeLabel) =
      props.proposalAligner.get.proposalType match {
        case Some(ProposalType.LargeProgram(_, _, _, _, _)) => ("Semester Max", "Semester Min")
        case _                                              => ("Max Time", "Min Time")
      }

    def makeMinimumPctInput[A](pctView: View[IntPercent], id: NonEmptyString): TagMod =
      FormInputTextView(
        value = pctView,
        validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
        changeAuditor = ChangeAuditor.refinedInt[ZeroTo100](),
        label = React.Fragment("Minimum %", HelpIcon("proposal/main/minimum-pct.md".refined)),
        id = id,
        disabled = props.readonly,
        inputClass = ExploreStyles.PartnerSplitsGridMinPct
      )

    def timeSplits(total: CalculatedValue[TimeSpan]) =
      val tagmod = partnerSplitsView.foldMap(_.get) match {
        case a if a.isEmpty => TagMod.empty
        case splits         =>
          sortedSplits(splits)
            .toTagMod(using ps => timeSplit(ps, total))
      }
      <.div(tagmod, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)

    val timeFields = if (areTimesSame) {
      // If min and max time are the same we only show one line
      val validTime = maxExecution.value > TimeSpan.Zero
      if (validTime) {
        React.Fragment(
          FormStaticData(
            value = formatHours(toHours(maxExecution.value)),
            label = "Prog. Time",
            id = "programTime",
            tooltip = maxExecution.staleTooltip
          )(maxExecution.staleClass),
          React.Fragment(
            timeSplits(maxExecution),
            minimumPct1View.map(r => minimumTime(r.get, maxExecution))
          )
        )
      } else
        React.Fragment(
          <.span, // ugly
          <.div(
            ExploreStyles.PartnerSplitsMissing,
            Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
            "No observations included in proposal."
          )
        )
    } else {
      React.Fragment(
        // The second partner splits row, for maximum times - is always there
        FormStaticData(
          value = formatHours(toHours(maxExecution.value)),
          label = maxTimeLabel,
          id = "maxTime",
          tooltip = maxExecution.staleTooltip
        )(maxExecution.staleClass),
        React.Fragment(
          timeSplits(maxExecution),
          minimumPct1View.map(r => minimumTime(r.get, maxExecution))
        ),
        // The third partner splits row, for minimum times - is always there
        FormStaticData(
          value = formatHours(toHours(minExecution.value)),
          label = minTimeLabel,
          id = "maxTime",
          tooltip = minExecution.staleTooltip
        )(minExecution.staleClass),
        React.Fragment(
          timeSplits(minExecution),
          minimumPct1View.map(r => minimumTime(r.get, minExecution))
        )
      )
    }

    extension (limits: SiteCoordinatesLimits)
      def format: String =
        val raStart  = limits.raStart.toHourAngle.toDoubleHours
        val raEnd    = limits.raEnd.toHourAngle.toDoubleHours
        val decStart = limits.decStart.toAngle.toSignedDoubleDegrees
        val decEnd   = limits.decEnd.toAngle.toSignedDoubleDegrees
        f"$raStart%.1fh ≤ RA ≤ $raEnd%.1fh    $decStart%.0f° ≤ Dec ≤ $decEnd%.0f°"

    <.form(
      <.div(ExploreStyles.ProposalDetailsGrid)(
        <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
          // Title input
          FormInputTextView(
            id = "title".refined,
            inputClass = Css("inverse"),
            groupClass = ExploreStyles.WarningInput.when_(titleView.get.isEmpty && !props.readonly),
            value = titleView,
            validFormat = InputValidSplitEpi.nonEmptyString.optional,
            label = "Title",
            disabled = props.readonly
          )(^.autoFocus := true),
          // Category selector
          FormDropdownOptional(
            id = "category".refined,
            label = React.Fragment("Category", HelpIcon("proposal/main/category.md".refined)),
            value = categoryView.get.map(categoryTag),
            options = categoryOptions,
            onChange = _.map(v => categoryView.set(Enumerated[TacCategory].fromTag(v))).orEmpty,
            disabled = props.readonly,
            modifiers = List(^.id := "category"),
            clazz = ExploreStyles.WarningInput.when_(categoryView.get.isEmpty && !props.readonly)
          ),
          activationView.map(activationView =>
            FormEnumDropdownView(
              id = "too-activation".refined,
              value = activationView,
              label = React.Fragment(
                "ToO Activation",
                HelpIcon("proposal/main/too-activation.md".refined)
              ),
              disabled = props.readonly
            )
          ),
          minimumPct1View.map(mv =>
            <.div(
              ExploreStyles.PartnerSplitsGrid,
              // Two optional items for proposal button and flags
              partnerSplitsView
                .map(psView =>
                  React.Fragment(
                    // The first partner splits row, with the button and the flags
                    <.div(
                      <.label("Partners"),
                      <.div(
                        Button(
                          icon = Icons.Edit,
                          severity = Button.Severity.Secondary,
                          tpe = Button.Type.Button,
                          onClick = showDialog.set(Visible.Shown),
                          tooltip = "Edit Partner Splits",
                          disabled = props.readonly
                        ).mini.compact
                      )
                    ),
                    PartnerSplitsEditor(
                      showDialog.get,
                      splitsList,
                      showDialog.set(Visible.Hidden),
                      splits => psView.set(splits)
                    ),
                    partnerSplits(psView.get)
                  )
                ),
              // Minimum percent total time - exists for most proposal types
              <.div(
                ExploreStyles.PartnerSplitsGridMinPctItem,
                makeMinimumPctInput(mv, "min-pct-1".refined)
              ),
              timeFields,
              // The third partner splits row - only exists for a few observation classes
              totalTimeView.map { totalTimeView =>
                def totalTimeEntry[A]: VdomNode =
                  FormInputTextView(
                    value = totalHours.withOnMod(h => totalTimeView.set(fromHours(h))),
                    validFormat = ExploreModelValidators.hoursValidWedge,
                    changeAuditor = ChangeAuditor.accept.decimal(2.refined),
                    label =
                      React.Fragment("Total", HelpIcon("proposal/main/total-time.md".refined)),
                    id = "total-time-entry".refined,
                    disabled = props.readonly,
                    inputClass = ExploreStyles.PartnerSplitsGridTotal
                  )

                val tt = totalTimeView.get

                React.Fragment(
                  <.div(totalTimeEntry),
                  timeSplits(tt.asReady),
                  <.div(
                    minimumPct2View
                      .map(makeMinimumPctInput(_, "min-pct-2".refined))
                      .orElse(minimumPct1View.map(v => minimumTime(v.get, tt.asReady)))
                      .getOrElse(EmptyVdom)
                  )
                )
              }
            )
          )
        ),
        <.div(
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            // Call for proposal selector
            FormDropdownOptional(
              id = "cfp".refined,
              label =
                React.Fragment("Call For Proposals", HelpIcon("proposal/main/cfp.md".refined)),
              value = selectedCfp,
              options = cfpOptions,
              onChange = _.map { cfp =>
                proposalCfpView.mod(
                  _.copy(
                    call = cfp.some,
                    proposalType = cfp.cfpType.defaultType(props.pi.map(_.id)).some
                  )
                )
              }.orEmpty,
              disabled = props.readonly,
              modifiers = List(^.id := "cfp"),
              clazz = ExploreStyles.WarningInput.when_(selectedCfp.isEmpty && !props.readonly)
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
              disabled = props.readonly,
              modifiers = List(^.id := "proposalType")
            ).when(hasSubtypes),
            // Reviewer and mentor for FastTurnaround
            props.proposal.proposalType
              .flatMap(ProposalType.fastTurnaround.getOption)
              .map: ft =>
                val reviewer = ft.reviewerId.flatMap(r => props.users.find(_.id === r))
                React.Fragment(
                  FormDropdownOptional(
                    id = "fastTurnaroundReviewer".refined,
                    label =
                      React.Fragment("Reviewer",
                                     HelpIcon("proposal/main/fast-turnaround-reviewer.md".refined)
                      ),
                    value = reviewer,
                    placeholder = props.pi.map(_.name).orUndefined,
                    options = props.users.map(u => SelectItem(u, u.name)),
                    onChange = props.setReviewer,
                    disabled = props.readonly
                  ),
                  // The API says it will default to the PI if the reviewer is null
                  Option.unless(reviewer.orElse(props.pi).exists(_.hasPhd)) {
                    val potentialMentors = props.users.filter(_.hasPhd)
                    // show icon or tooltip or something if there are no available mentors
                    FormDropdownOptional(
                      id = "fastTurnaroundMentor".refined,
                      label = "Mentor",
                      value = ft.mentorId.flatMap(r => props.users.find(_.id === r)),
                      options = potentialMentors.map(u => SelectItem(u, u.name)),
                      onChange = props.setMentor,
                      disabled = props.readonly
                    )
                  }
                )
          ),
          selectedCfp.map(cfp =>
            <.div(LucumaPrimeStyles.FormColumnVeryCompact, ExploreStyles.CfpData)(
              FormInfo(
                s"${GppDateFormatter.format(cfp.active.start)} to ${GppDateFormatter.format(cfp.active.end)}",
                "Observation Period"
              ),
              FormInfo(
                cfp.partners.map(_.partner.longName).mkString(", "),
                "Participating Partners"
              ),
              FormInfo(
                cfp.instruments.map(_.longName).mkString(", "),
                "Available Instruments"
              ),
              FormInfo(
                cfp.coordinateLimits.north.format,
                "Gemini North"
              ),
              FormInfo(
                cfp.coordinateLimits.south.format,
                "Gemini South"
              )
            )
          )
        )
      )
    )
  }

  private val component = ScalaFnComponent[Props](props =>
    for {
      ctx        <- useContext(AppContext.ctx)
      totalHours <- useStateView:
                      // we need `Hours` for editing
                      props.proposal.proposalType
                        .flatMap(ProposalType.totalTime.getOption)
                        .map(toHours)
                        .getOrElse(Hours.unsafeFrom(0))
      showDialog <- useStateView(Visible.Hidden)           // show partner splits modal
      splitsList <- useStateView(List.empty[PartnerSplit]) // partner splits modal
      _          <- useEffectWithDeps((props.proposal.call.map(_.id), props.cfps)):
                      // Update the partner splits when a new callId is set
                      (callId, cfps) =>
                        callId.foldMap(cid =>
                          val currentSplits    = Proposal.proposalType.some
                            .andThen(ProposalType.partnerSplits)
                            .getOption(props.proposal)
                          val cfpPartners      = cfps
                            .find(_.id === cid)
                            .foldMap(_.partners.map(_.partner))
                          val proposalPartners = currentSplits.orEmpty.filter(_._2 > 0).map(_.partner)

                          if (proposalPartners.nonEmpty && proposalPartners.forall(cfpPartners.contains))
                            splitsList.set(currentSplits.orEmpty)
                          else
                            splitsList.set(cfpPartners.map(p => PartnerSplit(p, 0.refined)))
                        )
    } yield renderFn(
      props,
      totalHours,
      showDialog,
      splitsList
    )(using ctx.logger)
  )
case class ProposalDetailsTitle(undoer: Undoer, tileSize: TileSizeState, readonly: Boolean)
    extends ReactFnProps(ProposalDetailsTitle.component)

object ProposalDetailsTitle:
  private type Props = ProposalDetailsTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      if (props.tileSize === TileSizeState.Minimized) EmptyVdom
      else
        <.div(ExploreStyles.TitleUndoButtons)(UndoButtons(props.undoer, disabled = props.readonly))
