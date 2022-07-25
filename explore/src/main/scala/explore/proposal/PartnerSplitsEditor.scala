// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all._
import crystal.react.View
import crystal.react.ViewOpt
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.components.ui.FomanticStyles
import explore.components.ui.PartnerFlags
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ZeroTo100
import lucuma.core.validation._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input._
import monocle.function.Index
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._

final case class PartnerSplitsEditor(
  show:    Boolean,
  splits:  View[List[PartnerSplit]],
  closeMe: Callback,
  onSave:  List[PartnerSplit] => Callback
) extends ReactFnProps[PartnerSplitsEditor](PartnerSplitsEditor.component)

object PartnerSplitsEditor {
  protected type Props = PartnerSplitsEditor

  private def toolbar(p: Props): ModalActions =
    ModalActions(
      <.div(
        ExploreStyles.FlexContainer,
        <.div("Total must be 100.",
              ExploreStyles.PartnerSplitsEditorError,
              ExploreStyles.FlexGrow(1)
        )
          .unless(addsUpTo100(p)),
        Button(negative = true, onClick = p.closeMe)(^.tpe := "button")(
          "Cancel",
          ExploreStyles.FlexEnd
        ),
        Button(positive = true)("OK", ^.disabled := !addsUpTo100(p), ^.tpe := "submit")
      )
    )

  private def makeTableRows(p: Props): TagMod =
    p.splits.get.zipWithIndex.toTagMod { case (ps, idx) =>
      val splitView: ViewOpt[PartnerSplit] =
        p.splits.zoom[PartnerSplit](Index.index[List[PartnerSplit], Int, PartnerSplit](idx))

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
                value = splitView.zoom(PartnerSplit.percent),
                validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
                changeAuditor = ChangeAuditor.refinedInt[ZeroTo100]()
              ).withMods(
                ^.autoFocus := idx === 0
              )
            )
          )
        )
      )
    }

  private def total(p: Props)       = p.splits.get.map(_.percent.value).sum
  private def addsUpTo100(p: Props) = total(p) === 100

  protected val component = ScalaFnComponent[Props] { props =>
    def save =
      if (addsUpTo100(props)) props.onSave(props.splits.get) >> props.closeMe else Callback.empty

    Modal(size = ModalSize.Mini, open = props.show)(
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
            TableBody(makeTableRows(props)),
            TableFooter(
              TableRow(
                TableCell("Total"),
                TableCell(s"${total(props)}%"),
                FomanticStyles.RightAligned
              )
            )
          ),
          toolbar(props)
        )
      )
    )
  }
}
