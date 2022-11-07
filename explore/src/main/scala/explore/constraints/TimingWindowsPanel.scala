// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Show
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.common.TimingQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.TimingWindowEntry
import explore.model.formats.*
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.implicits.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.PrimeAutoHeightVirtualizedTable
import lucuma.ui.table.PrimeTable
import lucuma.ui.table.*
import monocle.Optional
import monocle.Traversal
import monocle.function.FilterIndex.filterIndex
import monocle.function.Index
import monocle.function.Index.*
import react.common.ReactFnProps
import react.common.Style
import react.datepicker.Datepicker
import react.primereact.*
import react.resizeDetector.hooks.*
import react.semanticui.elements.label.LabelPointing
import reactST.tanstackTableCore.mod.Column

import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration

case class TimingWindowsPanel(windows: View[List[TimingWindowEntry]])
    extends ReactFnProps(TimingWindowsPanel.component)

object TimingWindowsPanel:
  private type Props = TimingWindowsPanel

  private val ColDef = ColumnDef[TimingWindowEntry]

  def formatZDT(tw: ZonedDateTime): String    =
    s"${Constants.GppDateFormatter.format(tw)} @ ${Constants.GppTimeTZFormatter.format(tw)}"
  def openText(tw: TimingWindowEntry): String =
    s"Open on ${formatZDT(tw.startsOn)}"

  private given Show[TimingWindowEntry] = Show.show {
    case tw @ TimingWindowEntry(_, startsOn, false, _, _, _, _, _, Some(closeOn)) =>
      s"${openText(tw)} and close on ${formatZDT(closeOn)}"
    case tw @ TimingWindowEntry(_, startsOn, false, _, _, _, _, Some(openFor), _) =>
      s"${openText(tw)}, remain open for ${durationHM.reverseGet(openFor)}"
    case tw @ TimingWindowEntry(_, startsOn, true, _, _, _, _, _, _)              =>
      s"${openText(tw)} and remain open forever"
    case tw @ TimingWindowEntry(_, startsOn, false, _, _, _, _, _, _)             =>
      openText(tw)
  }

  private val DeleteColWidth = 40
  private val WindowColId    = "Timing window"
  private val DeleteColId    = "Delete"

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      // cols
      .useMemoBy((_, _, resize) => resize) { (props, _, _) => resize =>
        List(
          ColDef(
            ColumnId(WindowColId),
            _.show,
            size = resize.width.map(z => (z - DeleteColWidth).toPx).getOrElse(400.toPx)
          ),
          ColDef(
            ColumnId(DeleteColId),
            size = DeleteColWidth.toPx
          ).setCell(_ => Icons.Trash)
        )
      }
      // rows
      .useMemoBy((props, _, _, _) => props.windows.get)((_, _, _, _) => l => l)
      .useReactTableBy((props, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          enableRowSelection = true,
          getRowId = (row, _, _) => RowId(row.id.toString)
        )
      )
      .render { (props, _, resize, _, rows, table) =>
        val current = table.getSelectedRowModel().rows.headOption.map(_.original)
        val pos     = rows.indexWhere((x: TimingWindowEntry) => current.exists(_.id === x.id))

        def headOption[A]: Optional[List[A], A] =
          Optional[List[A], A](_.headOption) { a =>
            {
              case x :: xs => a :: xs
              case Nil     => Nil
            }
          }
        println(current)

        val selectedTW            =
          props.windows.zoom(
            Index.index[List[TimingWindowEntry], Int, TimingWindowEntry](pos)
          )
        val selectedStartsOn      = selectedTW.zoom(TimingWindowEntry.startsOn)
        val selectedCloseOn       = selectedTW.zoom(TimingWindowEntry.closeOn)
        val selectedRemainOpenFor = selectedTW.zoom(TimingWindowEntry.remainOpenFor)
        val selectedRepeat        = selectedTW.zoom(TimingWindowEntry.repeat)
        val selectedRepeatTimes   = selectedTW.zoom(TimingWindowEntry.repeatTimes)

        <.div(
          ExploreStyles.TimingWindowsBody,
          <.div(
            ExploreStyles.TimingWindowsTable,
            PrimeAutoHeightVirtualizedTable(
              table,
              _ => 32.toPx,
              striped = true,
              compact = Compact.Very,
              cellMod = c =>
                c.column.id match {
                  case WindowColId =>
                    ^.style := Style(
                      Map("width" -> resize.width.foldMap(z => z - DeleteColWidth).px)
                    ).toJsObject
                  case DeleteColId =>
                    ^.style := Style(Map("width" -> DeleteColWidth.px)).toJsObject
                  case _           =>
                    TagMod.empty
                },
              headerMod = ExploreStyles.TimingWindowsHeader,
              containerMod = ExploreStyles.TimingWindowsTable,
              tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
              rowMod = row =>
                TagMod(
                  ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                  ^.onClick --> table.toggleAllRowsSelected(false) *>
                    Callback(row.toggleSelected())
                ),
              // If cmd is pressed add to the selection
              emptyMessage = "No timing windows defined"
            )
          ).withRef(resize.ref),
          current
            .map(e =>
              <.div(
                ExploreStyles.TimingWindowEditor,
                <.span(
                  "Open on ",
                  Datepicker(onChange =
                    (newValue, _) =>
                      newValue.fromDatePickerToZDTOpt.foldMap { i =>
                        selectedStartsOn.set(i.withSecond(0).withNano(0))
                      }
                  )
                    .showTimeInput(true)
                    .selected(e.startsOn.toDatePickerJsDate)
                    .dateFormat("yyyy-MM-dd HH:mm"),
                  <.span(" UTC and:")
                ),
                <.div(
                  RadioButton("forever",
                              id = "forever-option",
                              checked = e.forever,
                              onChange = (_, checked) => selectedTW.mod(_.toForever).when_(checked)
                  ),
                  <.label("Remain open forever", ^.htmlFor := "forever-option")
                ),
                <.div(
                  RadioButton(
                    "close-on",
                    id = "close-on-option",
                    checked = e.closeOn.isDefined,
                    onChange = (_, checked) =>
                      CallbackTo.now
                        .flatMap(i =>
                          selectedTW.mod(_.toCloseOn(ZonedDateTime.ofInstant(i, Constants.UTC)))
                        )
                        .when_(checked)
                  ),
                  <.label("Close on ", ^.htmlFor := "close-on-option"),
                  e.closeOn.map { closeOn =>
                    React.Fragment(
                      Datepicker(onChange =
                        (newValue, _) =>
                          newValue.fromDatePickerToZDTOpt.foldMap { i =>
                            selectedCloseOn.set(i.withSecond(0).withNano(0).some)
                          }
                      )
                        .showTimeInput(true)
                        .selected(closeOn.toDatePickerJsDate)
                        .dateFormat("yyyy-MM-dd HH:mm"),
                      <.span(" UTC")
                    )
                  }
                ),
                <.div(
                  ExploreStyles.TimingWindowRemainOpen,
                  RadioButton("remain-open",
                              id = "remain-open",
                              checked = e.remainOpenFor.isDefined,
                              onChange =
                                (_, checked) => selectedTW.mod(_.toRemainOpen).when_(checked)
                  ),
                  <.label("Remain open for", ^.htmlFor := "remain-open"),
                  selectedRemainOpenFor.asView.filter(_.get.isDefined).map { remainOpen =>
                    FormInputEV(
                      id = "remain-duration".refined,
                      value = remainOpen,
                      validFormat = durationHM.optional,
                      changeAuditor = ChangeAuditor.fromInputValidWedge(durationHM),
                      clazz = ExploreStyles.TargetRaDecMinWidth,
                      errorPointing = LabelPointing.Below,
                      errorClazz = ExploreStyles.InputErrorTooltip
                    )
                  }
                ),
                selectedRepeat.asView
                  .map(selectedRepeat =>
                    <.div(
                      ExploreStyles.TimingWindowRepeatEditor,
                      CheckboxView("repeat-with-period".refined,
                                   selectedRepeat,
                                   "Repeat with a period of"
                      )
                    )
                  )
                  .when(e.remainOpenFor.isDefined)
              ),
            ),
          Button(
            size = Button.Size.Small,
            onClick = CallbackTo.now
              .flatMap(i =>
                props.windows.mod(l =>
                  TimingWindowEntry.forever(
                    i.toEpochMilli().toInt,
                    ZonedDateTime.ofInstant(i, Constants.UTC).withSecond(0).withNano(0)
                  ) :: l
                )
              )
          ).compact
            .small(Icons.ThinPlus)
        )
      }
