// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.model.ZeroTo100
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.Message
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.*
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import explore.model.PartnerSplit

case class PartnerSplitsEditor(
  show:    Boolean,
  splits:  View[List[PartnerSplit]],
  closeMe: Callback,
  onSave:  List[PartnerSplit] => Callback
) extends ReactFnProps[PartnerSplitsEditor](PartnerSplitsEditor.component)

object PartnerSplitsEditor {
  private type Props = PartnerSplitsEditor

  private val ColDef = ColumnDef[View[PartnerSplit]]

  given Reusability[PartnerSplit] = Reusability.byEq

  private def save(props: Props) =
    if (addsUpTo100(props.splits.get))
      props.onSave(props.splits.get) >> props.closeMe
    else Callback.empty

  private def footer(props: Props) =
    <.div(
      Message(text = "Must add up to 100", severity = Message.Severity.Error)
        .unless(addsUpTo100(props.splits.get)),
      Button(label = "Cancel", severity = Button.Severity.Danger, onClick = props.closeMe),
      Button(label = "OK",
             severity = Button.Severity.Success,
             onClick = save(props),
             disabled = !addsUpTo100(props.splits.get)
      )
    )

  private def makeId(partner: Partner) = s"${partner.tag}-percent"
  private def makePartnerCell(partner: Partner): VdomNode =
    <.label(
      <.img(^.src        := PartnerFlags.smallFlag(partner),
            ^.alt := s"${partner.shortName} Flag",
            ExploreStyles.PartnerSplitFlag
      ),
      partner.longName,
      ^.htmlFor := makeId(partner)
    )

  private def makePercentCell(split: View[PartnerSplit]) =
    <.span(
      FormInputTextView(
        id = NonEmptyString.from(makeId(split.get.partner)).getOrElse("SPLIT_ID".refined),
        value = split.zoom(PartnerSplit.percent),
        validFormat = InputValidSplitEpi.refinedInt[ZeroTo100],
        changeAuditor = ChangeAuditor.refinedInt[ZeroTo100]()
      ).withMods(
        ^.autoFocus := split.get.partner === Partner.AR
      )
    )

  private def total(splits:       List[PartnerSplit]) = splits.map(_.percent.value).sum
  private def addsUpTo100(splits: List[PartnerSplit]) = total(splits) === 100

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // columns
    .useMemo(()) { _ =>
      List(
        ColDef(id = ColumnId("partner"),
               accessor = _.get.partner,
               header = "Partner",
               cell = cell => makePartnerCell(cell.value),
               footer = _ => "Total"
        ),
        ColDef(
          id = ColumnId("percent"),
          header = "Percent",
          cell = cell => makePercentCell(cell.row.original),
          footer = ctx =>
            val tot: Int = ctx.table.getRowModel().rows.map(_.original.get.percent.value).sum
            s"${tot}%"
        )
      )
    }
    // rows
    .useMemoBy((props, _) => props.splits.reuseByValue)((_, _) => _.value.toListOfViews)
    .useReactTableBy((_, cols, rows) =>
      TableOptions(cols,
                   rows,
                   getRowId = (row, _, _) => RowId(row.get.partner.tag),
                   enableSorting = false,
                   enableColumnResizing = false
      )
    )
    .render { (props, _, _, table) =>
      Dialog(
        visible = props.show,
        onHide = props.closeMe,
        header = "PartnerSplits",
        footer = footer(props),
        closable = false,
        resizable = false,
        focusOnShow = true,
        clazz = ExploreStyles.PartnerSplitsEditorDialog
      )(
        PrimeTable(table,
                   striped = true,
                   compact = Compact.Very,
                   tableMod = ExploreStyles.ExploreBorderTable
        )
      )
    }
}
