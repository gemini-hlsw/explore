// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all._
import coulomb._
import coulomb.accepted._
import coulomb.refined._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.AppCtx
import explore.Icons
import explore.components.FormStaticData
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui._
import explore.implicits._
import explore.model._
import explore.model.display._
import explore.model.enum.ProposalClass._
import explore.model.refined._
import explore.model.reusability._
import japgolly.scalajs.react.Reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Partner
import lucuma.ui.forms._
import lucuma.ui.optics._
import lucuma.ui.reusability._
import monocle.Lens
import monocle.macros.Lenses
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.addons.textarea.TextArea
import react.semanticui.collections.form._
import react.semanticui.elements.label.Label
import spire.std.any._

final case class ProposalDetailsEditor(proposalDetails: View[ProposalDetails])
    extends ReactProps[ProposalDetailsEditor](ProposalDetailsEditor.component)

object ProposalDetailsEditor {
  type Props = ProposalDetailsEditor

  @Lenses
  final case class State(showPartnerSplitsModal: Boolean, splits: List[PartnerSplit])

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  private def formatTime(time: Double) = f"$time%.2fh"

  private def sortedSplits(splits: List[PartnerSplit]) =
    splits.sortBy(_.percent.value.value)(Ordering[Int].reverse)

  private def partnerSplits(splits: List[PartnerSplit]): TagMod = splits match {
    case Nil =>
      <.span(
        Icons.ExclamationTriangle,
        "Partner time allocations are required.",
        ExploreStyles.TextInForm,
        FomanticStyles.WarningText
      )
    case _   =>
      val ps = sortedSplits(splits)
        .toTagMod(ps => partnerSplit(ps))
      <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  }

  private def partnerSplit(ps: PartnerSplit): TagMod = {
    val id   = s"${ps.partner.tag}-split"
    val text = f"${ps.percent.value.value}%%"
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
      ExploreStyles.FlexShrink(0),
      ExploreStyles.PartnerSplitData
    )
  }

  private def timeSplits(splits: List[PartnerSplit], total: NonNegHour): TagMod = splits match {
    case Nil => TagMod.empty
    case _   =>
      val ps = sortedSplits(splits)
        .toTagMod(ps => timeSplit(ps, total))
      <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  }

  private def timeSplit(ps: PartnerSplit, total: NonNegHour) = {
    val splitTime = ps.percent.to[Double, Unitless] * total
    val timeText  = formatTime(splitTime.value)
    <.span(timeText, ExploreStyles.TextInForm, ExploreStyles.PartnerSplitData)
  }

  private def minimumTime(pct: IntPercent, total: NonNegHour) = {
    val time     = pct.to[Double, Unitless] * total
    val timeText = formatTime(time.value)
    <.span(timeText, ExploreStyles.TextInForm, ExploreStyles.MinimumPercent)
  }

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.using { implicit appCtx =>
        val splitsZoom = ViewF.fromState[IO]($).zoom(State.splits)

        val details      = props.proposalDetails
        val requestTime1 = details.zoom(ProposalDetails.requestTime1).get
        val requestTime2 = details.zoom(ProposalDetails.requestTime2).get
        val minimumPct1  = details.zoom(ProposalDetails.minimumPct1).get
        val minimumPct2  = details.zoom(ProposalDetails.minimumPct1).get

        val (time1Label, time2Label, hasSecondTime, has2Minimums) =
          details.get.proposalClass match {
            case Queue                    => ("Band 1 & 2", "Band 3", true, false)
            case LargeProgram | Intensive => ("1st Semester", "Total", true, true)
            case _                        => ("Time", "", false, false)
          }

        def closePartnerSplitsEditor: Callback =
          $.modState(State.showPartnerSplitsModal.set(false))

        def saveStateSplits(splits: List[PartnerSplit]): Callback =
          details
            .zoom(ProposalDetails.partnerSplits)
            .set(splits.filter(_.percent.value.value > 0))
            .runAsyncCB *> closePartnerSplitsEditor

        def openPartnerSplitsEditor: Callback = {
          val splits      = details.get.partnerSplits
          val allPartners = Partner.EnumeratedPartner.all.map(p =>
            splits
              .find(_.partner === p)
              .getOrElse(PartnerSplit(p, 0.withRefinedUnit[ZeroTo100, Percent]))
          )
          $.setState(State(true, allPartners))
        }

        def makeMinimumPctInput[A](lens: Lens[ProposalDetails, IntPercent]) =
          FormInputEV(
            value = details.zoom(lens).stripQuantity,
            validFormat = ValidFormatInput.forRefinedInt[ZeroTo100](),
            changeAuditor = ChangeAuditor.forRefinedInt[ZeroTo100](),
            label = Label("Minimum %", HelpIcon("proposal/main/minimum-pct.md")),
            id = "minimum-pct"
          ).withMods(
            ExploreStyles.FlexShrink(0),
            ExploreStyles.MinimumPercent
          )

        <.div(
          <.div(
            <.div(
              ^.key := "details",
              ExploreStyles.ProposalTile,
              Tile("Details")(
                Form(
                  <.div(
                    ExploreStyles.TwoColumnGrid,
                    ExploreStyles.ProposalDetailsGrid,
                    FormInputEV(
                      id = "title",
                      className = "inverse",
                      value = details.zoom(ProposalDetails.title),
                      label = "Title"
                    ).withMods(^.autoFocus := true),
                    EnumViewSelect(id = "proposal-class",
                                   value = details.zoom(ProposalDetails.proposalClass),
                                   label = Label("Class", HelpIcon("proposal/main/class.md"))
                    ),
                    <.div(
                      ExploreStyles.FlexContainer,
                      FormButton(
                        icon = Icons.Edit,
                        label = "Partners",
                        tpe = "button",
                        clazz = ExploreStyles.FlexShrink(0) |+| ExploreStyles.PartnerSplitTotal,
                        onClick = openPartnerSplitsEditor
                      ),
                      partnerSplits(details.zoom(ProposalDetails.partnerSplits).get),
                      makeMinimumPctInput(ProposalDetails.minimumPct1).unless(has2Minimums)
                    ),
                    EnumViewOptionalSelect(id = "category",
                                           value = details.zoom(ProposalDetails.category),
                                           label = Label("Category",
                                                         HelpIcon("proposal/main/category.md")
                                           )
                    ),
                    <.div(
                      ExploreStyles.FlexContainer,
                      FormStaticData(value = formatTime(requestTime1.value.value),
                                     label = time1Label,
                                     id = "time1"
                      )(
                        ExploreStyles.FlexShrink(0),
                        ExploreStyles.PartnerSplitTotal
                      ),
                      timeSplits(details.zoom(ProposalDetails.partnerSplits).get, requestTime1),
                      minimumTime(minimumPct1, requestTime1).unless(has2Minimums),
                      makeMinimumPctInput(ProposalDetails.minimumPct1).when(has2Minimums)
                    ),
                    EnumViewMultipleSelect(
                      id = "keywords",
                      value = details.zoom(ProposalDetails.keywords),
                      label = "Keywords",
                      search = true
                    ),
                    <.div(
                      ExploreStyles.FlexContainer,
                      FormStaticData(value = formatTime(requestTime2.value.value),
                                     label = time2Label,
                                     id = "time2"
                      )(
                        ExploreStyles.FlexShrink(0),
                        ExploreStyles.PartnerSplitTotal
                      ),
                      timeSplits(details.zoom(ProposalDetails.partnerSplits).get, requestTime2),
                      minimumTime(minimumPct2, requestTime2).unless(has2Minimums),
                      makeMinimumPctInput(ProposalDetails.minimumPct2).when(has2Minimums)
                    ).when(hasSecondTime),
                    <.span().unless(hasSecondTime),
                    EnumViewSelect(id = "too-activation",
                                   value = details.zoom(ProposalDetails.toOActivation),
                                   label = Label("ToO Activation",
                                                 HelpIcon("proposal/main/too-activation.md")
                                   )
                    )
                  ),
                  <.div(FomanticStyles.Divider),
                  FormTextArea(
                    label = "Abstract",
                    rows = 10,
                    value = details.zoom(ProposalDetails.abstrakt).get,
                    onChangeE = (_: TextArea.ReactChangeEvent, tap: TextArea.TextAreaProps) => {
                      details
                        .zoom(ProposalDetails.abstrakt)
                        .set(tap.value.asInstanceOf[String])
                        .runAsyncCB
                    }
                  ).addModifiers(Seq(^.id := "abstract"))
                )
              ),
              <.div(
                ^.key := "preview",
                ExploreStyles.ProposalTile,
                Tile("Preview")(
                  <.span("Placeholder for PDF preview.")
                )
              ),
              PartnerSplitsEditor(state.showPartnerSplitsModal,
                                  splitsZoom,
                                  closePartnerSplitsEditor,
                                  saveStateSplits
              )
            )
          )
        )
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(false, Nil))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
