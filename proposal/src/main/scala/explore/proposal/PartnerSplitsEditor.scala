// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import eu.timepit.refined.auto._
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Partner
import mouse.all._
import react.common.ReactProps
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._

final case class PartnerSplitsEditor(
  show:         Boolean,
  splits:       List[PartnerSplit],
  closeMe:      Callback,
  updateSplits: List[PartnerSplit] => Callback,
  onSave:       List[PartnerSplit] => Callback
) extends ReactProps[PartnerSplitsEditor](PartnerSplitsEditor.component)

object PartnerSplitsEditor {
  type Props = PartnerSplitsEditor

  implicit val propsReuse: Reusability[Props] = Reusability.by(p => (p.show, p.splits))

  private def toolbar(p: Props): ModalActions =
    ModalActions(
      <.div(
        ExploreStyles.FlexContainer,
        <.div("Total must be 100.",
              ExploreStyles.PartnerSplitsEditorError,
              ExploreStyles.FlexGrow(1)
        )
          .unless(addsUpTo100(p)),
        Button(onClick = p.closeMe)(^.tpe := "button")("Cancel", ExploreStyles.FlexEnd),
        Button(^.tpe := "submit")("OK", ^.disabled := !addsUpTo100(p))
      )
    )

  private def updatePercent(p: Props, partner: Partner)(e: ReactEventFromInput): Callback = {
    val newValue =
      if (e.target.value === "") "0"
      else e.target.value
    newValue.parseIntOption
      .flatMap(_ match {
        case i if i >= 0 && i <= 100 => Some(i)
        case _                       => None
      })
      .map { pct =>
        p.splits.map { s =>
          if (s.partner === partner) s.copy(percent = pct)
          else s
        }
      }
      .fold(Callback.empty)(p.updateSplits)
  }

  private def makeTableRows(p: Props): TagMod =
    p.splits.zipWithIndex.toTagMod { case (ps, idx) =>
      val id = s"${ps.partner.tag}-percent"
      React.Fragment(
        <.tr(
          <.td(
            <.label(
              <.img(^.src := PartnerFlags.smallFlag(ps.partner),
                    ^.alt := s"${ps.partner.name} Flag",
                    ExploreStyles.PartnerSplitFlag
              ),
              ps.partner.name,
              ^.htmlFor := id
            )
          ),
          <.td(
            <.input(
              ^.id := id,
              ^.value := ps.percent,
              ^.onChange ==> updatePercent(p, ps.partner),
              ^.autoFocus := idx === 0
            )
          )
        )
      )
    }

  private def total(p:       Props) = p.splits.map(_.percent).sum
  private def addsUpTo100(p: Props) = total(p) === 100

  def render(p: Props): VdomNode = {
    def save = if (addsUpTo100(p)) p.onSave(p.splits) else Callback.empty

    Modal(
      size = ModalSize.Mini,
      open = p.show
    )(
      ModalHeader("Partner Splits"),
      ModalContent(
        Form(
          action = "#",
          onSubmitE = (e: ReactEvent, _: Form.FormProps) => e.preventDefaultCB *> save
        )(
          <.table(
            ExploreStyles.PartnerSplitsEditorTable,
            <.thead(
              <.tr(
                <.th("Partner"),
                <.th("Percent")
              )
            ),
            <.tbody(makeTableRows(p)),
            <.tfoot(
              <.tr(<.td("Total"), <.td(s"${total(p)}%"))
            )
          ),
          toolbar(p)
        )
      )
    )
  }

  val component = ScalaComponent
    .builder[Props]
    // .renderBackend[Backend]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
