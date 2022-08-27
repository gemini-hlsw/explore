// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order._
import cats.data.Chain
import cats.effect.IO
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import crystal.react.View
import crystal.react.hooks._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.FormStaticData
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui._
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ExploreModelValidators
import explore.model.Hours
import explore.model.display.given
import explore.model.reusability._
import explore.optics.all._
import explore.proposal.ProposalClassType._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums._
import lucuma.core.model.IntPercent
import lucuma.core.model.NonNegDuration
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.ProposalClass
import lucuma.core.model.ZeroTo100
import lucuma.core.syntax.time._
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms._
import lucuma.ui.input._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.common.ProgramQueriesGQL
import queries.schemas.implicits._
import react.common.ReactFnProps
import react.semanticui.collections.form._
import react.semanticui.elements.label.Label
import react.semanticui.modules.dropdown._
import react.semanticui.shorthand._
import spire.std.any._

import scala.collection.immutable.SortedMap

import scalajs.js.JSConverters._

final case class ProposalEditor(
  programId:     Program.Id,
  proposal:      View[Proposal],
  undoStacks:    View[UndoStacks[IO, Proposal]],
  executionTime: NonNegDuration,
  band3Time:     NonNegDuration
)(using val ctx: AppContextIO)
    extends ReactFnProps[ProposalEditor](ProposalEditor.component)

object ProposalEditor {
  type Props = ProposalEditor

  private val Hours2Micros = BigDecimal(60L * 60L * 1000L * 1000L)

  private def toHours(time: NonNegDuration): Hours =
    Hours.from(time.value.toMicros / Hours2Micros).getOrElse(Hours.Max)

  private def fromHours(hours: Hours): NonNegDuration =
    NonNegDuration.unsafeFrom((hours.value * Hours2Micros).toLong.microseconds)

  private def formatHours(hours: BigDecimal) = f"$hours%.2fh"

  private def sortedSplits(splits: SortedMap[Partner, IntPercent]): List[PartnerSplit] =
    splits.toList
      .map { case (part, perc) => PartnerSplit(part, perc) }
      .sortBy(_.percent.value)(Ordering[Int].reverse)

  private def partnerSplits(splits: SortedMap[Partner, IntPercent]): TagMod = splits match {
    case a if a.isEmpty =>
      <.span(
        Icons.ExclamationTriangle,
        "Partner time allocations are required.",
        FomanticStyles.WarningText
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
      <.img(^.src := PartnerFlags.smallFlag(partner),
            ^.alt := s"${partner.name}  Flag",
            ExploreStyles.PartnerSplitFlag
      )
    val span: TagMod = <.span(data)

    FormStaticData(id = id, value = <.div(img, span), label = partner.shortName)(
      ExploreStyles.FlexShrink(0.refined),
      ExploreStyles.PartnerSplitData
    )
  }

  private def timeSplits(splits: SortedMap[Partner, IntPercent], total: NonNegDuration): TagMod =
    splits match {
      case a if a.isEmpty => TagMod.empty
      case _              =>
        val ps = sortedSplits(splits)
          .toTagMod(ps => timeSplit(ps, total))
        <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
    }

  private def timeSplit(ps: PartnerSplit, total: NonNegDuration) = {
    val splitTime = ps.percent.value * toHours(total).value / 100
    val timeText  = formatHours(splitTime)
    <.span(timeText, ExploreStyles.PartnerSplitData)
  }

  private def minimumTime(pct: IntPercent, total: NonNegDuration) = {
    val time     = pct.value * toHours(total).value / 100
    val timeText = formatHours(time)
    <.span(timeText, ExploreStyles.MinimumPercent)
  }

  def categoryTag(category: TacCategory): String = Enumerated[TacCategory].tag(category)

  private val categoryOptions = Enumerated[TacCategory].all
    .groupBy(_.group)
    .toList
    .sortBy(_._1)
    .foldRight(Chain.empty[DropdownItem]) { case ((group, cats), acc) =>
      val groupItem =
        DropdownItem(text = group.label, value = group.label, className = "header", disabled = true)
      val catItems  =
        Chain.fromSeq(
          cats.map(cat => DropdownItem(text = cat.label, value = categoryTag(cat)))
        )
      groupItem +: (catItems ++ acc)
    }
    .toList

  private def saveStateSplits(
    splitView: View[SortedMap[Partner, IntPercent]],
    splitList: List[PartnerSplit]
  ): Callback =
    splitView
      .set(SortedMap.from(splitList.filter(_.percent.value > 0).map(_.toTuple)))

  def renderDetails(
    aligner:           Aligner[Proposal, Input[ProposalInput]],
    undoCtx:           UndoContext[Proposal],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showModal:         View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    splitsMap:         SortedMap[Partner, IntPercent],
    executionTime:     NonNegDuration,
    band3Time:         NonNegDuration,
    renderInTitle:     Tile.RenderInTitle
  )(implicit ctx:      AppContextIO): VdomNode = {

    val titleAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.title, t => _.map(ProposalInput.title.modify(t)))

    val titleView = titleAligner.view(_.orUnassign)

    val classAligner: Aligner[ProposalClass, Input[ProposalClassInput]] =
      aligner.zoom(Proposal.proposalClass, c => _.map(ProposalInput.proposalClass.modify(c)))

    val classView: View[ProposalClass] = classAligner.view(_.toInput.assign)

    val categoryAligner: Aligner[Option[TacCategory], Input[TacCategory]] =
      aligner.zoom(Proposal.category, c => _.map(ProposalInput.category.modify(c)))

    val categoryView: View[Option[TacCategory]] = categoryAligner.view(_.orUnassign)

    val activationAligner: Aligner[ToOActivation, Input[ToOActivation]] =
      aligner.zoom(Proposal.toOActivation, a => _.map(ProposalInput.toOActivation.modify(a)))

    val activationView: View[ToOActivation] = activationAligner.view(_.assign)

    val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
      aligner.zoom(Proposal.abstrakt, t => _.map(ProposalInput.`abstract`.modify(t)))

    val abstractView = abstractAligner.view(_.orUnassign)

    val totalTimeView   = classView.zoom(ProposalClass.totalTime)
    val totalTime       = totalTimeView.get
    val minimumPct1View = classView.zoom(ProposalClass.minPercentTime)
    val minimumPct2View = classView.zoom(ProposalClass.minPercentTotalTime)

    val has2Minimums = minimumPct2View.get.isDefined

    val (time1Label, secondTime) =
      ProposalClassType.fromProposalClass(classView.get) match {
        case Queue                    => ("Band 1 & 2", band3Time.some)
        case LargeProgram | Intensive => ("1st Semester", totalTime)
        case _                        => ("Time", none)
      }

    def makeMinimumPctInput[A](pctView: View[IntPercent], id: NonEmptyString): TagMod =
      FormInputEV(
        value = pctView,
        validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
        changeAuditor = ChangeAuditor.refinedInt[ZeroTo100](),
        label = Label("Minimum %", HelpIcon("proposal/main/minimum-pct.md".refined)),
        id = id
      ).withMods(
        ExploreStyles.FlexShrink(0.refined),
        ExploreStyles.MinimumPercent
      )

    def totalTimeEntry[A]: Option[VdomNode] =
      totalTime.map(_ =>
        FormInputEV(
          value = totalHours.withOnMod(h => totalTimeView.set(fromHours(h))),
          validFormat = ExploreModelValidators.hoursValidWedge,
          changeAuditor = ChangeAuditor.accept.decimal(2.refined),
          label = Label("Total", HelpIcon("proposal/main/total-time.md".refined)),
          id = "total-time-entry".refined
        ).withMods(
          ExploreStyles.FlexShrink(0.refined),
          ExploreStyles.PartnerSplitTotal
        )
      )

    def openPartnerSplitsEditor: Callback = {
      val allPartners = Partner.EnumeratedPartner.all.map(p =>
        splitsMap
          .get(p)
          .fold(PartnerSplit(p, 0.refined))(pct => PartnerSplit(p, pct))
      )
      splitsList.set(allPartners) >> showModal.set(true)
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
      Form(
        <.div(
          ExploreStyles.TwoColumnGrid,
          ExploreStyles.ProposalDetailsGrid,
          FormInputEV(
            id = "title".refined,
            className = "inverse",
            value = titleView,
            validFormat = InputValidSplitEpi.nonEmptyString.optional,
            label = "Title"
          ).withMods(^.autoFocus := true),
          <.div(
            ExploreStyles.FlexContainer,
            FormButton(
              icon = Icons.Edit,
              label = "Partners",
              tpe = "button",
              clazz = ExploreStyles.FlexShrink(0.refined) |+| ExploreStyles.PartnerSplitTotal,
              onClick = openPartnerSplitsEditor
            ),
            partnerSplits(splitsMap),
            makeMinimumPctInput(minimumPct1View, "min-pct-1".refined).unless(has2Minimums)
          ),
          <.div(
            ExploreStyles.FlexContainer,
            FormStaticData(value = formatHours(toHours(executionTime)),
                           label = time1Label,
                           id = "time1"
            )(
              ExploreStyles.FlexShrink(0.refined),
              ExploreStyles.PartnerSplitTotal
            ),
            timeSplits(splitsMap, executionTime),
            minimumTime(minimumPct1View.get, executionTime).unless(has2Minimums),
            makeMinimumPctInput(minimumPct1View, "min-pct-1".refined).when(has2Minimums)
          ),
          secondTime.fold(<.span(): VdomNode) { t2 =>
            <.div(
              ExploreStyles.FlexContainer,
              totalTimeEntry.getOrElse(
                FormStaticData(value = formatHours(toHours(t2)), label = "Band 3", id = "band-3")(
                  ExploreStyles.FlexShrink(0.refined),
                  ExploreStyles.PartnerSplitTotal
                )
              ),
              timeSplits(splitsMap, t2),
              minimumPct2View
                .mapValue(pctView => makeMinimumPctInput(pctView, "min-pct-2".refined))
                .getOrElse(minimumTime(minimumPct1View.get, t2))
            )
          },
          EnumViewSelect(
            id = "proposal-class",
            value = proposalClassType.withOnMod(onClassTypeMod _),
            label = Label("Class", HelpIcon("proposal/main/class.md".refined))
          ),
          FormSelect(
            label = Label("Category", HelpIcon("proposal/main/category.md".refined)),
            value = categoryView.get.map(categoryTag).orUndefined,
            options = categoryOptions,
            onChange = (ddp: FormDropdown.FormDropdownProps) =>
              ddp.value.toOption
                .map(v => categoryView.set(Enumerated[TacCategory].fromTag(v.asInstanceOf[String])))
                .orEmpty,
            modifiers = List(^.id := "category")
          ),
          EnumViewSelect(
            id = "too-activation",
            value = activationView,
            label = Label("ToO Activation", HelpIcon("proposal/main/too-activation.md".refined))
          )
        ),
        <.div(FomanticStyles.Divider),
        FormTextAreaEV(
          id = "abstract".refined,
          label = "Abstract",
          rows = 10,
          value = abstractView.as(optionNonEmptyStringIso)
        )
      )
    )
  }

  def renderFn(
    programId:         Program.Id,
    proposal:          View[Proposal],
    undoStacks:        View[UndoStacks[IO, Proposal]],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showModal:         View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    executionTime:     NonNegDuration,
    band3Time:         NonNegDuration
  )(implicit ctx:      AppContextIO) = {
    def closePartnerSplitsEditor: Callback = showModal.set(false)

    val undoCtx: UndoContext[Proposal]                   = UndoContext(undoStacks, proposal)
    val aligner: Aligner[Proposal, Input[ProposalInput]] =
      Aligner(
        undoCtx,
        UpdateProgramsInput(
          WHERE = programId.toWhereProgram.assign,
          SET = ProgramPropertiesInput(proposal = ProposalInput().assign)
        ),
        (ProgramQueriesGQL.UpdateProgramsMutation.execute[IO] _).andThen(_.void)
      ).zoom(Iso.id[Proposal].asLens,
             UpdateProgramsInput.SET.andThen(ProgramPropertiesInput.proposal).modify
      )

    val splitsAligner: Aligner[SortedMap[Partner, IntPercent], Input[List[PartnerSplitsInput]]] =
      aligner.zoom(Proposal.partnerSplits, s => _.map(ProposalInput.partnerSplits.modify(s)))

    val splitsView: View[SortedMap[Partner, IntPercent]] =
      splitsAligner.view(
        _.toList
          .map { case (par, pct) =>
            PartnerSplitsInput(partner = par.assign, percent = pct.assign)
          }
          .assign
      )

    <.div(
      <.div(
        ^.key := "details",
        ExploreStyles.ProposalTile,
        Tile("details".refined, "Details")(
          renderDetails(
            aligner,
            undoCtx,
            totalHours,
            minPct2,
            proposalClassType,
            showModal,
            splitsList,
            splitsView.get,
            executionTime,
            band3Time,
            _
          )
        )
      ),
      <.div(
        ^.key := "preview",
        ExploreStyles.ProposalTile,
        Tile("preview".refined, "Preview")(_ => <.span("Placeholder for PDF preview."))
      ),
      PartnerSplitsEditor(
        showModal.get,
        splitsList,
        closePartnerSplitsEditor,
        saveStateSplits(splitsView, _)
      )
    )
  }

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(
        // total time - we need `Hours` for editing and also to preserve if
        // the user switches between classes with and without total time.
        _.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.totalTime))
          .get
          .map(toHours)
          .getOrElse(Hours.unsafeFrom(0))
      )
      .useStateViewBy((props, _) =>
        // mininum percent total time = need to preserve between class switches
        props.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.minPercentTotalTime))
          .get
          .getOrElse(IntPercent.unsafeFrom(80))
      )
      .useStateViewBy((props, _, _) =>
        // Initial proposal class type
        ProposalClassType.fromProposalClass(props.proposal.get.proposalClass)
      )
      .useStateView(false) // show partner splits modal
      .useStateView(List.empty[PartnerSplit])
      .useStateViewBy((props, _, _, _, _, _) => props.proposal.get.proposalClass)
      .useEffectWithDepsBy((props, _, _, _, _, _, _) => props.proposal.get.proposalClass)(
        // Deal with changes to the ProposalClass.
        (props, totalHours, minPct2, classType, _, _, oldClass) =>
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
      .render { (props, totalHours, minPct2, proposalClassType, showModal, splitsList, _) =>
        import props.given
        renderFn(
          props.programId,
          props.proposal,
          props.undoStacks,
          totalHours,
          minPct2,
          proposalClassType,
          showModal,
          splitsList,
          props.executionTime,
          props.band3Time
        )
      }
}
