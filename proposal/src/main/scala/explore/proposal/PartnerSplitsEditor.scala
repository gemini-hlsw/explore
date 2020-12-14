// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.components.ui.FomanticStyles
import explore.components.ui.PartnerFlags
import explore.implicits._
import explore.model._
import explore.model.refined._
import explore.model.reusability._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics._
import react.common.ReactProps
import react.semanticui.collections.form.Form
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._

final case class PartnerSplitsEditor(
  show:    Boolean,
  splits:  View[List[PartnerSplit]],
  closeMe: Callback,
  onSave:  List[PartnerSplit] => Callback
) extends ReactProps[PartnerSplitsEditor](PartnerSplitsEditor.component)

object PartnerSplitsEditor {
  type Props = PartnerSplitsEditor

  implicit val propsReuse: Reusability[Props] = Reusability.by(p => (p.show, p.splits.get))

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

  private def makeTableRows(p: Props): TagMod =
    p.splits.get.zipWithIndex.toTagMod { case (ps, idx) =>
      // we're already looking at the one we want
      val getSplit: List[PartnerSplit] => PartnerSplit = _ => ps

      def modSplit(mod: PartnerSplit => PartnerSplit): List[PartnerSplit] => List[PartnerSplit] =
        list => list.modFirstWhere(_.partner === ps.partner, mod)

      def splitView: View[PartnerSplit] = p.splits.zoom[PartnerSplit](getSplit)(modSplit)

      val id = s"${ps.partner.tag}-percent"
      React.Fragment(
        TableRow(
          TableCell(
            <.label(
              <.img(^.src := PartnerFlags.smallFlag(ps.partner),
                    ^.alt := s"${ps.partner.name} Flag",
                    ExploreStyles.PartnerSplitFlag
              ),
              ps.partner.name,
              ^.htmlFor := id
            )
          ),
          TableCell(
            <.span(
              FormInputEV(
                id = NonEmptyString.from(id).getOrElse("SPLIT_ID"),
                value = splitView.zoom(PartnerSplit.percent).stripQuantity,
                validFormat = ValidFormatInput.forRefinedInt[ZeroTo100](),
                changeAuditor = ChangeAuditor.forRefinedInt[ZeroTo100]()
              ).withMods(
                ^.autoFocus := idx === 0
              )
            )
          )
        )
      )
    }

  private def total(p:       Props) = p.splits.get.map(_.percent.value.value).sum
  private def addsUpTo100(p: Props) = total(p) === 100

  def render(p: Props): VdomNode = {
    def save = if (addsUpTo100(p)) p.onSave(p.splits.get) else Callback.empty

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
          Table(compact = TableCompact.Very)(
            ExploreStyles.PartnerSplitsEditorTable,
            TableHeader(
              TableRow(
                TableHeaderCell("Partner"),
                TableHeaderCell("Percent")
              )
            ),
            TableBody(makeTableRows(p)),
            TableFooter(
              TableRow(TableCell("Total"), TableCell(s"${total(p)}%"), FomanticStyles.RightAligned)
            )
          ),
          toolbar(p)
        )
      )
    )
  }

  val component = ScalaComponent
    .builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
