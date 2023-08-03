// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Order.*
import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.Aligner
import explore.components.FormStaticData
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.*
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Hours
import explore.model.display.given
import explore.optics.all.*
import explore.proposal.ProposalClassType.*
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Proposal
import lucuma.core.model.ProposalClass
import lucuma.core.model.ZeroTo100
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL
import react.common.Css
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Divider
import react.primereact.SelectItem
import spire.std.any.*

import scala.collection.immutable.SortedMap

case class ProposalEditor(
  programId:     Program.Id,
  proposal:      View[Proposal],
  undoStacks:    View[UndoStacks[IO, Proposal]],
  executionTime: Option[TimeSpan],
  band3Time:     TimeSpan
) extends ReactFnProps(ProposalEditor.component)

object ProposalEditor:
  private type Props = ProposalEditor

  private val Hours2Micros = BigDecimal(60L * 60L * 1000L * 1000L)

  private def toHours(time: TimeSpan): Hours =
    Hours.from(time.toHours.longValue).getOrElse(Hours.Max)

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

  private def timeSplits(splits: SortedMap[Partner, IntPercent], total: TimeSpan): TagMod =
    splits match {
      case a if a.isEmpty => TagMod.empty
      case _              =>
        val ps = sortedSplits(splits)
          .toTagMod(ps => timeSplit(ps, total))
        <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
    }

  private def timeSplit(ps: PartnerSplit, total: TimeSpan) = {
    val splitTime = ps.percent.value * toHours(total).value / 100
    val timeText  = formatHours(splitTime)
    <.span(timeText, ExploreStyles.PartnerSplitData)
  }

  private def minimumTime(pct: IntPercent, total: TimeSpan) = {
    val time     = pct.value * toHours(total).value / 100
    val timeText = formatHours(time)
    <.span(timeText, ExploreStyles.MinimumPercent)
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
    aligner:           Aligner[Proposal, Input[ProposalInput]],
    undoCtx:           UndoContext[Proposal],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showDialog:        View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    splitsMap:         SortedMap[Partner, IntPercent],
    executionTime:     TimeSpan,
    band3Time:         TimeSpan,
    renderInTitle:     Tile.RenderInTitle
  )(using Logger[IO]): VdomNode = {
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
      FormInputTextView(
        value = pctView,
        validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
        changeAuditor = ChangeAuditor.refinedInt[ZeroTo100](),
        label = React.Fragment("Minimum %", HelpIcon("proposal/main/minimum-pct.md".refined)),
        id = id
      )

    def totalTimeEntry[A]: Option[VdomNode] =
      totalTime.map(_ =>
        FormInputTextView(
          value = totalHours.withOnMod(h => totalTimeView.set(fromHours(h))),
          validFormat = ExploreModelValidators.hoursValidWedge,
          changeAuditor = ChangeAuditor.accept.decimal(2.refined),
          label = React.Fragment("Total", HelpIcon("proposal/main/total-time.md".refined)),
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
              label = "Title"
            )(^.autoFocus := true),
            <.div(
              ExploreStyles.FlexContainer,
              <.div(ExploreStyles.FlexShrink(0.refined) |+| ExploreStyles.PartnerSplitTotal)(
                <.label("Partners"),
                Button(
                  icon = Icons.Edit,
                  severity = Button.Severity.Secondary,
                  tpe = Button.Type.Button,
                  onClick = openPartnerSplitsEditor
                )
              ),
              partnerSplits(splitsMap),
              <.div(ExploreStyles.FlexShrink(0.refined), ExploreStyles.MinimumPercent)(
                makeMinimumPctInput(minimumPct1View, "min-pct-1".refined).unless(has2Minimums)
              )
            ),
            <.div(
              ExploreStyles.FlexContainer,
              FormStaticData(
                value = formatHours(toHours(executionTime)),
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
            }
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, LucumaPrimeStyles.LinearColumn)(
            FormEnumDropdownView(
              id = "proposal-class".refined,
              value = proposalClassType.withOnMod(onClassTypeMod _),
              label = React.Fragment("Class", HelpIcon("proposal/main/class.md".refined))
            ),
            FormDropdownOptional(
              id = "category".refined,
              label = React.Fragment("Category", HelpIcon("proposal/main/category.md".refined)),
              value = categoryView.get.map(categoryTag),
              options = categoryOptions,
              onChange = _.map(v => categoryView.set(Enumerated[TacCategory].fromTag(v))).orEmpty,
              modifiers = List(^.id := "category")
            ),
            FormEnumDropdownView(
              id = "too-activation".refined,
              value = activationView,
              label = React.Fragment("ToO Activation",
                                     HelpIcon("proposal/main/too-activation.md".refined)
              )
            )
          )
        ),
        Divider(borderType = Divider.BorderType.Solid),
        FormInputTextAreaView(
          id = "abstract".refined,
          label = "Abstract",
          value = abstractView.as(optionNonEmptyStringIso)
        )(ExploreStyles.ProposalAbstract, ^.rows := 10)
      )
    )
  }

  private def renderFn(
    programId:         Program.Id,
    proposal:          View[Proposal],
    undoStacks:        View[UndoStacks[IO, Proposal]],
    totalHours:        View[Hours],
    minPct2:           View[IntPercent],
    proposalClassType: View[ProposalClassType],
    showDialog:        View[Boolean],
    splitsList:        View[List[PartnerSplit]],
    executionTime:     TimeSpan,
    band3Time:         TimeSpan
  )(using FetchClient[IO, ObservationDB], Logger[IO]) = {
    def closePartnerSplitsEditor: Callback = showDialog.set(false)

    val undoCtx: UndoContext[Proposal]                   = UndoContext(undoStacks, proposal)
    val aligner: Aligner[Proposal, Input[ProposalInput]] =
      Aligner(
        undoCtx,
        UpdateProgramsInput(
          WHERE = programId.toWhereProgram.assign,
          SET = ProgramPropertiesInput(proposal = ProposalInput().assign)
        ),
        (ProgramQueriesGQL.UpdateProgramsMutation[IO].execute(_)).andThen(_.void)
      ).zoom(Iso.id[Proposal].asLens,
             UpdateProgramsInput.SET.andThen(ProgramPropertiesInput.proposal).modify
      )

    val splitsAligner: Aligner[SortedMap[Partner, IntPercent], Input[List[PartnerSplitInput]]] =
      aligner.zoom(Proposal.partnerSplits, s => _.map(ProposalInput.partnerSplits.modify(s)))

    val splitsView: View[SortedMap[Partner, IntPercent]] =
      splitsAligner.view(
        _.toList
          .map { case (par, pct) =>
            PartnerSplitInput(partner = par, percent = pct)
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
            showDialog,
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
        showDialog.get,
        splitsList,
        closePartnerSplitsEditor,
        saveStateSplits(splitsView, _)
      )
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateViewBy((props, _) =>
        // total time - we need `Hours` for editing and also to preserve if
        // the user switches between classes with and without total time.
        props.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.totalTime))
          .get
          .map(toHours)
          .getOrElse(Hours.unsafeFrom(0))
      )
      .useStateViewBy((props, _, _) =>
        // mininum percent total time = need to preserve between class switches
        props.proposal
          .zoom(Proposal.proposalClass.andThen(ProposalClass.minPercentTotalTime))
          .get
          .getOrElse(IntPercent.unsafeFrom(80))
      )
      .useStateViewBy((props, _, _, _) =>
        // Initial proposal class type
        ProposalClassType.fromProposalClass(props.proposal.get.proposalClass)
      )
      .useStateView(false) // show partner splits modal
      .useStateView(List.empty[PartnerSplit])
      .useStateViewBy((props, _, _, _, _, _, _) => props.proposal.get.proposalClass)
      .useEffectWithDepsBy((props, _, _, _, _, _, _, _) => props.proposal.get.proposalClass)(
        // Deal with changes to the ProposalClass.
        (props, _, totalHours, minPct2, classType, _, _, oldClass) =>
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
      .render { (props, ctx, totalHours, minPct2, proposalClassType, showDialog, splitsList, _) =>
        import ctx.given

        renderFn(
          props.programId,
          props.proposal,
          props.undoStacks,
          totalHours,
          minPct2,
          proposalClassType,
          showDialog,
          splitsList,
          props.executionTime.getOrElse(
            TimeSpan.Zero
          ), // this is not correct, we need to be able to handle a missing execution time
          props.band3Time
        )
      }
