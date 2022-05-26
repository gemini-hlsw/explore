// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.proposal

// import cats.Order._
// import cats.data.Chain
// import cats.syntax.all._
// import coulomb._
// import coulomb.accepted._
// import coulomb.refined._
// import crystal.react.ReuseView
// import crystal.react.reuse._
// import eu.timepit.refined.auto._
// import eu.timepit.refined.cats._
// import explore.Icons
// import explore.components.FormStaticData
// import explore.components.HelpIcon
// import explore.components.Tile
// import explore.components.ui._
// import explore.implicits._
// import explore.model._
// import explore.model.display._
// import explore.model.refined._
// import explore.model.reusability._
// import japgolly.scalajs.react.Reusability._
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.vdom.html_<^._
// import lucuma.core.enum.ProposalClass._
// import lucuma.core.enum._
// import lucuma.core.model.Partner
// import lucuma.core.util.Enumerated
// import lucuma.ui.forms._
// import lucuma.ui.optics._
// import lucuma.ui.reusability._
// import monocle.Focus
// import monocle.Lens
// import react.common.ReactProps
// import react.common.implicits._
// import react.semanticui.addons.textarea.TextArea
// import react.semanticui.collections.form._
// import react.semanticui.elements.label.Label
// import react.semanticui.modules.dropdown._
// import react.semanticui.shorthand._
// import spire.std.any._

// import scala.annotation.unused

// import scalajs.js.JSConverters._

// final case class ProposalDetailsEditor(proposalDetails: ReuseView[ProposalDetails])
//     extends ReactProps[ProposalDetailsEditor](ProposalDetailsEditor.component)

// object ProposalDetailsEditor {
//   type Props = ProposalDetailsEditor

//   final case class State(showPartnerSplitsModal: Boolean, splits: List[PartnerSplit])

//   object State {
//     val showPartnerSplitsModal = Focus[State](_.showPartnerSplitsModal)

//     val splits = Focus[State](_.splits)
//   }

//   implicit val propsReuse: Reusability[Props] = Reusability.derive
//   implicit val stateReuse: Reusability[State] = Reusability.derive

//   private def formatTime(time: Double) = f"$time%.2fh"

//   private def sortedSplits(splits: List[PartnerSplit]) =
//     splits.sortBy(_.percent.value.value)(Ordering[Int].reverse)

//   private def partnerSplits(splits: List[PartnerSplit]): TagMod = splits match {
//     case Nil =>
//       <.span(
//         Icons.ExclamationTriangle,
//         "Partner time allocations are required.",
//         FomanticStyles.WarningText
//       )
//     case _   =>
//       val ps = sortedSplits(splits)
//         .toTagMod(ps => partnerSplit(ps))
//       <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
//   }

//   private def partnerSplit(ps: PartnerSplit): TagMod = {
//     val id   = s"${ps.partner.tag}-split"
//     val text = f"${ps.percent.value.value}%%"
//     partnerSplitData(ps.partner, id, text)
//   }

//   private def partnerSplitData(partner: Partner, id: String, data: String) = {
//     val img: TagMod  =
//       <.img(^.src := PartnerFlags.smallFlag(partner),
//             ^.alt := s"${partner.name}  Flag",
//             ExploreStyles.PartnerSplitFlag
//       )
//     val span: TagMod = <.span(data)

//     FormStaticData(id = id, value = <.div(img, span), label = partner.shortName)(
//       ExploreStyles.FlexShrink(0),
//       ExploreStyles.PartnerSplitData
//     )
//   }

//   private def timeSplits(splits: List[PartnerSplit], total: NonNegHour): TagMod = splits match {
//     case Nil => TagMod.empty
//     case _   =>
//       val ps = sortedSplits(splits)
//         .toTagMod(ps => timeSplit(ps, total))
//       <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
//   }

//   private def timeSplit(ps: PartnerSplit, total: NonNegHour) = {
//     val splitTime = ps.percent.to[Double, Unitless] * total
//     val timeText  = formatTime(splitTime.value)
//     <.span(timeText, ExploreStyles.PartnerSplitData)
//   }

//   private def minimumTime(pct: IntPercent, total: NonNegHour) = {
//     val time     = pct.to[Double, Unitless] * total
//     val timeText = formatTime(time.value)
//     <.span(timeText, ExploreStyles.MinimumPercent)
//   }

//   private val categoryOptions = Enumerated[TacCategory].all
//     .groupBy(_.group)
//     .toList
//     .sortBy(_._1)
//     .foldRight(Chain.empty[DropdownItem]) { case ((group, cats), acc) =>
//       val groupItem =
//         DropdownItem(text = group.label, value = group.label, className = "header", disabled = true)
//       val catItems  =
//         Chain.fromSeq(cats.map(cat => DropdownItem(text = cat.label, value = cat.label)))
//       groupItem +: (catItems ++ acc)
//     }
//     .toList

//   class Backend($ : BackendScope[Props, State]) {

//     def renderDetails(
//       details:               ReuseView[ProposalDetails],
//       @unused renderInTitle: Tile.RenderInTitle
//     ): VdomNode = {
//       val requestTime1 = details.zoom(ProposalDetails.requestTime1).get
//       val requestTime2 = details.zoom(ProposalDetails.requestTime2).get
//       val minimumPct1  = details.zoom(ProposalDetails.minimumPct1).get
//       val minimumPct2  = details.zoom(ProposalDetails.minimumPct1).get

//       val (time1Label, time2Label, hasSecondTime, has2Minimums) =
//         details.get.proposalClass match {
//           case Queue                    => ("Band 1 & 2", "Band 3", true, false)
//           case LargeProgram | Intensive => ("1st Semester", "Total", true, true)
//           case _                        => ("Time", "", false, false)
//         }

//       def makeMinimumPctInput[A](lens: Lens[ProposalDetails, IntPercent]) =
//         FormInputEV(
//           value = details.zoom(lens).stripQuantity,
//           validFormat = ValidFormatInput.forRefinedInt[ZeroTo100](),
//           changeAuditor = ChangeAuditor.forRefinedInt[ZeroTo100](),
//           label = Label("Minimum %", HelpIcon("proposal/main/minimum-pct.md")),
//           id = "minimum-pct"
//         ).withMods(
//           ExploreStyles.FlexShrink(0),
//           ExploreStyles.MinimumPercent
//         )

//       def openPartnerSplitsEditor: Callback = {
//         val splits      = details.get.partnerSplits
//         val allPartners = Partner.EnumeratedPartner.all.map(p =>
//           splits
//             .find(_.partner === p)
//             .getOrElse(PartnerSplit(p, 0.withRefinedUnit[ZeroTo100, Percent]))
//         )
//         $.setState(State(true, allPartners))
//       }

//       Form(
//         <.div(
//           ExploreStyles.TwoColumnGrid,
//           ExploreStyles.ProposalDetailsGrid,
//           FormInputEV[ReuseView, String](
//             id = "title",
//             className = "inverse",
//             value = details.zoom(ProposalDetails.title),
//             label = "Title"
//           ).withMods(^.autoFocus := true),
//           <.div(
//             ExploreStyles.FlexContainer,
//             FormButton(
//               icon = Icons.Edit,
//               label = "Partners",
//               tpe = "button",
//               clazz = ExploreStyles.FlexShrink(0) |+| ExploreStyles.PartnerSplitTotal,
//               onClick = openPartnerSplitsEditor
//             ),
//             partnerSplits(details.zoom(ProposalDetails.partnerSplits).get),
//             makeMinimumPctInput(ProposalDetails.minimumPct1).unless(has2Minimums)
//           ),
//           <.div(
//             ExploreStyles.FlexContainer,
//             FormStaticData(value = formatTime(requestTime1.value.value),
//                            label = time1Label,
//                            id = "time1"
//             )(
//               ExploreStyles.FlexShrink(0),
//               ExploreStyles.PartnerSplitTotal
//             ),
//             timeSplits(details.zoom(ProposalDetails.partnerSplits).get, requestTime1),
//             minimumTime(minimumPct1, requestTime1).unless(has2Minimums),
//             makeMinimumPctInput(ProposalDetails.minimumPct1).when(has2Minimums)
//           ),
//           <.div(
//             ExploreStyles.FlexContainer,
//             FormStaticData(value = formatTime(requestTime2.value.value),
//                            label = time2Label,
//                            id = "time2"
//             )(
//               ExploreStyles.FlexShrink(0),
//               ExploreStyles.PartnerSplitTotal
//             ),
//             timeSplits(details.zoom(ProposalDetails.partnerSplits).get, requestTime2),
//             minimumTime(minimumPct2, requestTime2).unless(has2Minimums),
//             makeMinimumPctInput(ProposalDetails.minimumPct2).when(has2Minimums)
//           ).when(hasSecondTime),
//           <.span().unless(hasSecondTime),
//           EnumViewSelect[ReuseView, ProposalClass](
//             id = "proposal-class",
//             value = details.zoom(ProposalDetails.proposalClass),
//             label = Label("Class", HelpIcon("proposal/main/class.md"))
//           ),
//           FormSelect(
//             label = Label("Category", HelpIcon("proposal/main/category.md")),
//             value = details.get.category.map(_.label).orUndefined,
//             options = categoryOptions,
//             onChange = (ddp: FormDropdown.FormDropdownProps) =>
//               ddp.value.toOption
//                 .map(v =>
//                   details
//                     .zoom(ProposalDetails.category)
//                     .set(Enumerated[TacCategory].fromTag(v.asInstanceOf[String]))
//                 )
//                 .orEmpty,
//             modifiers = List(^.id := "category")
//           ),
//           EnumViewSelect[ReuseView, ToOActivation](
//             id = "too-activation",
//             value = details.zoom(ProposalDetails.toOActivation),
//             label = Label("ToO Activation", HelpIcon("proposal/main/too-activation.md"))
//           )
//         ),
//         <.div(FomanticStyles.Divider),
//         FormTextArea(
//           label = "Abstract",
//           rows = 10,
//           value = details.zoom(ProposalDetails.abstrakt).get,
//           onChangeE = (_: TextArea.ReactChangeEvent, tap: TextArea.TextAreaProps) =>
//             details
//               .zoom(ProposalDetails.abstrakt)
//               .set(tap.value.asInstanceOf[String])
//         ).addModifiers(Seq(^.id := "abstract"))
//       )
//     }

//     def render(props: Props, state: State) = {
//       val splitsZoom = ReuseView.fromState($).zoom(State.splits)

//       val details = props.proposalDetails

//       def closePartnerSplitsEditor: Callback =
//         $.modState(State.showPartnerSplitsModal.replace(false))

//       def saveStateSplits(
//         details: ReuseView[ProposalDetails],
//         splits:  List[PartnerSplit]
//       ): Callback =
//         details
//           .zoom(ProposalDetails.partnerSplits)
//           .set(splits.filter(_.percent.value.value > 0)) >>
//           closePartnerSplitsEditor

//       <.div(
//         <.div(
//           ^.key := "details",
//           ExploreStyles.ProposalTile,
//           Tile("details", "Details")(Reuse(renderDetails _)(details))
//         ),
//         <.div(
//           ^.key := "preview",
//           ExploreStyles.ProposalTile,
//           Tile("preview", "Preview")(Reuse.always(_ => <.span("Placeholder for PDF preview.")))
//         ),
//         PartnerSplitsEditor(
//           state.showPartnerSplitsModal,
//           splitsZoom,
//           closePartnerSplitsEditor,
//           Reuse(saveStateSplits _)(details)
//         )
//       )
//     }
//   }

//   val component =
//     ScalaComponent
//       .builder[Props]
//       .initialState(State(false, Nil))
//       .renderBackend[Backend]
//       .configure(Reusability.shouldComponentUpdate)
//       .build
// }
