// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import coulomb._
import coulomb.accepted._
import coulomb.refined._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore._
import explore.model.reusability._
import explore.components.FormStaticData
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model._
import explore.model.display._
import explore.model.PartnerSplit._
import explore.model.ProposalDetails._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.Reusability._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Partner
import lucuma.ui.forms._
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common.implicits._
import react.common.ReactProps
import react.semanticui.addons.textarea.TextArea
import react.semanticui.collections.form._
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

  private def partnerSplits(splits: List[PartnerSplit]): TagMod = {
    val ps = sortedSplits(splits)
      .toTagMod(ps => partnerSplit(ps))
    <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  }

  private def partnerSplit(ps: PartnerSplit): TagMod = {
    val id   = s"${ps.partner.tag}-split"
    val text = f"${ps.percent.value.value}%%"
    partnerSplitData(ps.partner, id, text)
  }

  // TODO: switch to use partner.shortName after next lucuma-core release
  private def partnerSplitData(partner: Partner, id: String, data: String) = {
    val img: TagMod  =
      <.img(^.src := PartnerFlags.smallFlag(partner),
            ^.alt := s"${partner.name}  Flag",
            ExploreStyles.PartnerSplitFlag
      )
    val span: TagMod = <.span(data)

    FormStaticData(id = id, value = <.div(img, span), label = partner.name)(
      ExploreStyles.FlexShrink(0),
      ExploreStyles.PartnerSplitData
    )
  }

  private def bandSplits(splits: List[PartnerSplit], total: NonNegHour) = {
    val ps = sortedSplits(splits)
      .toTagMod(ps => bandSplit(ps, total))
    <.div(ps, ExploreStyles.FlexContainer, ExploreStyles.FlexWrap)
  }

  private def bandSplit(ps: PartnerSplit, total: NonNegHour) = {
    // TODO: switch to ps.abbreviation when available in next lucuma-core release
    val partnerText = ps.partner.tag.toUpperCase
    val splitTime   = ps.percent.to[Double, Unitless] * total
    val timeText    = formatTime(splitTime.value)
    <.span(s"$partnerText $timeText", ExploreStyles.TextInForm, ExploreStyles.PartnerSplitData)
  }

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      val details        = props.proposalDetails
      val band1And2Hours = details.zoom(ProposalDetails.band1And2Hours).get
      val band3Hours     = details.zoom(ProposalDetails.band3Hours).get

      def updateStateSplits(splits: List[PartnerSplit]): Callback =
        $.setStateL(State.splits)(splits)

      def closePartnerSplitsEditor: Callback =
        $.modState(State.showPartnerSplitsModal.set(false))

      def saveStateSplits(splits: List[PartnerSplit]): Callback =
        details
          .zoom(ProposalDetails.partnerSplits)
          .set(splits.filter(_.percent.value.value > 0))
          .runInCB *> closePartnerSplitsEditor

      def openPartnerSplitsEditor: Callback = {
        val splits      = details.get.partnerSplits
        val allPartners = Partner.EnumeratedPartner.all.map(p =>
          splits
            .find(_.partner === p)
            .getOrElse(PartnerSplit(p, 0.withRefinedUnit[ZeroTo100, Percent]))
        )
        $.setState(State(true, allPartners))
      }

      <.div(
        <.div(
          <.div(
            ^.key := "details",
            ExploreStyles.ProposalTile,
            Tile("Details", movable = false)(
              Form(
                ExploreStyles.TwoColumnGrid,
                FormInputEV(
                  id = "title",
                  className = "inverse",
                  value = details.zoom(ProposalDetails.title),
                  label = "Title"
                ).withMods(^.autoFocus := true),
                EnumViewSelect(id = "proposal-class",
                               value = details.zoom(ProposalDetails.proposalClass).asOpt,
                               label = "Class"
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  FormButton(
                    icon = Icons.Edit,
                    label = "Partners",
                    clazz = ExploreStyles.FlexShrink(0) |+| ExploreStyles.PartnerSplitTotal,
                    onClick = openPartnerSplitsEditor
                  ),
                  partnerSplits(details.zoom(ProposalDetails.partnerSplits).get)
                ),
                EnumViewSelect(id = "category",
                               value = details.zoom(ProposalDetails.category),
                               label = "Category"
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  FormStaticData(value = formatTime(band1And2Hours.value.value),
                                 label = "Band 1 & 2",
                                 id = "band1-2"
                  )(
                    ExploreStyles.FlexShrink(0),
                    ExploreStyles.PartnerSplitTotal
                  ),
                  bandSplits(details.zoom(ProposalDetails.partnerSplits).get, band1And2Hours)
                ),
                EnumViewMultipleSelect(
                  id = "keywords",
                  value = details.zoom(ProposalDetails.keywords),
                  label = "Keywords",
                  search = true
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  FormStaticData(value = formatTime(band3Hours.value.value),
                                 label = "Band 3",
                                 id = "band3"
                  )(
                    ExploreStyles.FlexShrink(0),
                    ExploreStyles.PartnerSplitTotal
                  ),
                  bandSplits(details.zoom(ProposalDetails.partnerSplits).get, band3Hours)
                ),
                EnumViewSelect(id = "too-activation",
                               value = details.zoom(ProposalDetails.toOActivation),
                               label = "ToO Activation"
                )
              )
            )
          ),
          <.div(
            ^.key := "abstract",
            ExploreStyles.ProposalTile,
            Tile("Abstract", movable = false)(
              Form(
                TextArea(
                  rows = 10,
                  value = details.zoom(ProposalDetails.abstrakt).get,
                  onChangeE = (_: TextArea.ReactChangeEvent, tap: TextArea.TextAreaProps) => {
                    details
                      .zoom(ProposalDetails.abstrakt)
                      .set(tap.value.asInstanceOf[String])
                      .runInCB
                  }
                )
              )
            )
          )
        ),
        <.div(
          ^.key := "preview",
          ExploreStyles.ProposalTile,
          Tile("Preview", movable = false)(
            <.span("Placeholder for PDF preview.")
          )
        ),
        PartnerSplitsEditor(state.showPartnerSplitsModal,
                            state.splits,
                            closePartnerSplitsEditor,
                            updateStateSplits,
                            saveStateSplits
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
