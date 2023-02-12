// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import eu.timepit.refined.cats.*
import explore.Icons
import explore.common.TimingQueries
import explore.common.TimingQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.TimingWindow
import explore.model.TimingWindowRepeat
import explore.model.TimingWindowRepeatPeriod
import explore.model.formats.*
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.given
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.util.*
import lucuma.ui.table.PrimeAutoHeightVirtualizedTable
import lucuma.ui.table.PrimeTable
import lucuma.ui.table.*
import monocle.Optional
import monocle.Traversal
import monocle.function.FilterIndex.filterIndex
import monocle.function.Index
import monocle.function.Index.*
import queries.common.TimingWindowsGQL.*
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import react.common.ReactFnProps
import react.common.Style
import react.datepicker.Datepicker
import react.primereact.*
import react.resizeDetector.hooks.*
import reactST.tanstackTableCore.mod.Column

import java.time.Duration
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration

import scalajs.js.timers

case class TimingWindowsPanel(windows: View[List[TimingWindow]])
    extends ReactFnProps(TimingWindowsPanel.component)

private object TimingWindowOperating extends NewType[Boolean]:
  inline def Operating: TimingWindowOperating = TimingWindowOperating(true)
  inline def Idle: TimingWindowOperating      = TimingWindowOperating(false)
  extension (s: TimingWindowOperating)
    inline def flip: TimingWindowOperating =
      if (s.value) TimingWindowOperating.Idle else TimingWindowOperating.Operating
private type TimingWindowOperating = TimingWindowOperating.Type

object TimingWindowsPanel:
  private type Props = TimingWindowsPanel

  private val ColDef = ColumnDef[TimingWindow]

  private def formatZDT(tw: ZonedDateTime): String =
    s"${Constants.GppDateFormatter.format(tw)} @ ${Constants.GppTimeTZFormatter.format(tw)}"
  private def openText(tw: TimingWindow): String   =
    s"Open on ${formatZDT(tw.startsOn)}"

  private given Show[TimingWindowRepeatPeriod] = Show.show {
    case TimingWindowRepeatPeriod(period, None)                     =>
      s"repeat with a period of ${durationHMS.reverseGet(period)} forever"
    case TimingWindowRepeatPeriod(period, Some(n)) if n.value === 1 =>
      s"repeat with a period of ${durationHMS.reverseGet(period)} once"
    case TimingWindowRepeatPeriod(period, Some(n))                  =>
      s"repeat with a period of ${durationHMS.reverseGet(period)} $n times"
  }

  private given Show[TimingWindowRepeat] = Show.show {
    case TimingWindowRepeat(period, None)         =>
      s"remain open for ${durationHM.reverseGet(period)}"
    case TimingWindowRepeat(period, Some(repeat)) =>
      s"remain open for ${durationHM.reverseGet(period)}, ${repeat.show}"
  }

  private given Show[TimingWindow] = Show.show {
    case tw @ TimingWindow(_, startsOn, Some(Right(repeat))) =>
      s"${openText(tw)}, ${repeat.show}"
    case tw @ TimingWindow(_, startsOn, Some(Left(closeOn))) =>
      s"${openText(tw)} and close on ${formatZDT(closeOn)}"
    case tw @ TimingWindow(_, startsOn, None)                =>
      s"${openText(tw)} and remain open forever"
  }

  private val DeleteColWidth = 20
  private val WindowColId    = "Timing window"
  private val DeleteColId    = "Delete"

  private def deleteRow(
    dbActive: TimingWindowOperating => Callback,
    id:       Int,
    windows:  View[List[TimingWindow]]
  )(using TransactionalClient[IO, UserPreferencesDB]): Callback =
    dbActive(TimingWindowOperating.Operating) *>
      DeleteTimingWindow
        .execute(id.assign)
        .flatMap(_ => windows.mod(_.filterNot(_.id === id)).to[IO])
        .guarantee(dbActive(TimingWindowOperating.Idle).to[IO])
        .runAsyncAndForget

  private def addNewRow(
    dbActive: TimingWindowOperating => Callback,
    windows:  View[List[TimingWindow]]
  )(using TransactionalClient[IO, UserPreferencesDB]): Callback =
    dbActive(TimingWindowOperating.Operating) *> CallbackTo.now
      .flatMap { i =>
        val startsOn =
          ZonedDateTime.ofInstant(i, ZoneOffset.UTC).withSecond(0).withNano(0)

        InsertTimingWindow
          .execute(startsOn.assign)
          .flatMap(id =>
            id.insertTmpTimingWindowsOne
              .map(_.id)
              .map(id =>
                windows
                  .mod(l => (TimingWindow.forever(id, startsOn) :: l.reverse).reverse)
                  .to[IO]
              )
              .orEmpty
          )
          .guarantee(dbActive(TimingWindowOperating.Idle).to[IO])
          .runAsyncAndForget
      }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      .useState(TimingWindowOperating.Idle)
      // cols
      .useMemoBy((_, _, resize, operating) => (resize, operating)) {
        (props, ctx, _, _) => (resize, operating) =>
          import ctx.given
          List(
            ColDef(
              ColumnId(WindowColId),
              _.show,
              size = resize.width.map(z => (z - DeleteColWidth).toPx).getOrElse(400.toPx)
            ),
            ColDef(
              ColumnId(DeleteColId),
              identity,
              size = DeleteColWidth.toPx
            ).setCell(c =>
              Button(text = true,
                     onClickE = e =>
                       e.stopPropagationCB *> deleteRow(
                         operating.setState,
                         c.value.id,
                         props.windows
                       )
              ).compact.small(Icons.Trash)
            )
          )
      }
      // rows
      .useMemoBy((props, _, _, _, _) => props.windows.get)((_, _, _, _, _) => l => l)
      .useReactTableBy((props, _, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          enableRowSelection = true,
          getRowId = (row, _, _) => RowId(row.id.toString)
        )
      )
      .render { (props, ctx, resize, dbActive, _, rows, table) =>
        import ctx.given

        val current = table.getSelectedRowModel().rows.headOption.map(_.original)
        val pos     = rows.indexWhere((x: TimingWindow) => current.exists(_.id === x.id))

        val selectedTW =
          props.windows
            .zoom(Index.index[List[TimingWindow], Int, TimingWindow](pos))
            .withOnMod {
              case Some(tw) =>
                TimingQueries.updateTimingWindow[IO](tw).runAsyncAndForget
              case None     => Callback.empty
            }

        val selectedStartsOn      = selectedTW.zoom(TimingWindow.startsOn)
        val selectedCloseOn       = selectedTW.zoom(TimingWindow.closeOn)
        val selectedRemainOpenFor = selectedTW.zoom(TimingWindow.remainOpenFor)
        val selectedRepeatPeriod  = selectedTW.zoom(TimingWindow.repeatPeriod)
        val selectedRepeatNTimes  = selectedTW.zoom(TimingWindow.repeatFrequency)

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
              emptyMessage = <.div(ExploreStyles.ExploreTableEmpty, "No timing windows defined")
            )
          ).withRef(resize.ref),
          current
            .map { e =>
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
                              checked = e.openForever,
                              onChange = (_, checked) => selectedTW.mod(_.toForever).when_(checked)
                  ),
                  <.label("Remain open forever", ^.htmlFor := "forever-option")
                ),
                <.div(
                  RadioButton(
                    "close-on",
                    id = "close-on-option",
                    checked = e.closeOn,
                    onChange = (_, checked) =>
                      CallbackTo.now
                        .flatMap(i =>
                          selectedTW.mod(_.toCloseOn(ZonedDateTime.ofInstant(i, ZoneOffset.UTC)))
                        )
                        .when_(checked)
                  ),
                  <.label("Close on ", ^.htmlFor := "close-on-option"),
                  selectedCloseOn.asView.map { closeOn =>
                    React.Fragment(
                      Datepicker(onChange =
                        (newValue, _) =>
                          newValue.fromDatePickerToZDTOpt.foldMap { i =>
                            selectedCloseOn.set(i.withSecond(0).withNano(0))
                          }
                      )
                        .showTimeInput(true)
                        .selected(closeOn.get.toDatePickerJsDate)
                        .dateFormat("yyyy-MM-dd HH:mm"),
                      <.span(" UTC")
                    )
                  }
                ),
                <.div(
                  ExploreStyles.TimingWindowRemainOpen,
                  RadioButton(
                    "remain-open",
                    id = "remain-open",
                    checked = e.remainOpenFor,
                    onChange = (_, checked) =>
                      selectedTW
                        .mod(
                          _.toRemainOpen(TimeSpan.unsafeFromDuration(Duration.ofDays(2)))
                        )
                        .when_(checked)
                  ),
                  <.label("Remain open for", ^.htmlFor := "remain-open"),
                  selectedRemainOpenFor.asView.map { remainOpen =>
                    FormInputTextView(
                      id = "remain-duration".refined,
                      value = remainOpen,
                      validFormat = durationHM,
                      changeAuditor = ChangeAuditor.fromInputValidWedge(durationHM)
                    )
                  }
                ),
                selectedRemainOpenFor.asView
                  .map { _ =>
                    <.div(
                      <.div(
                        ExploreStyles.TimingWindowRepeatEditor,
                        <.div(
                          LucumaStyles.CheckboxWithLabel,
                          Checkbox(
                            id = "repeat-with-period",
                            checked = e.repeatPeriod,
                            onChange = checked =>
                              selectedTW
                                .mod(
                                  _.toRepeatPeriod(
                                    TimeSpan.unsafeFromDuration(Duration.ofHours(12))
                                  )
                                )
                                .when_(checked) *>
                                selectedTW.mod(_.noRepeatPeriod).unless_(checked)
                          ),
                          <.label("Repeat with a period of", ^.htmlFor := "repeat-with-period")
                        ),
                        FormInputTextView(
                          id = "repat-period".refined,
                          value = selectedRepeatPeriod,
                          validFormat = durationHMS,
                          changeAuditor = ChangeAuditor.fromInputValidWedge(durationHMS),
                          disabled = !e.repeatPeriod
                        )
                      ),
                      <.div(
                        ExploreStyles.TimingWindowRepeatEditorAlternatives,
                        <.div(
                          RadioButton(
                            "repeat-forever",
                            id = "repeat-forever-option",
                            checked = e.repeatForever,
                            onChange =
                              (_, checked) => selectedTW.mod(_.toRepeatPeriodForever).when_(checked)
                          ),
                          <.label("Forever", ^.htmlFor := "repeat-forever-option")
                        ),
                        <.div(
                          ExploreStyles.TimingWindowRepeatEditorNTimes,
                          RadioButton(
                            "repeat-n-times",
                            id = "repeat-n-times",
                            checked = !e.repeatForever,
                            onChange = (_, checked) =>
                              selectedTW.mod(_.toRepeatPeriodNTimes(1.refined)).when_(checked)
                          ),
                          FormInputTextView(
                            id = "repeat-n-times-value".refined,
                            value = selectedRepeatNTimes,
                            validFormat = InputValidSplitEpi.posInt,
                            changeAuditor = ChangeAuditor.posInt,
                            disabled = !e.repeatPeriod,
                            // when focusing, if the viewOpt is empty, switch to `n times` and select all text
                            onFocus = ev =>
                              selectedRepeatNTimes.get.fold(
                                selectedTW.mod(_.toRepeatPeriodNTimes(1.refined)) >>
                                  Callback(timers.setTimeout(0)(ev.target.select()))
                              )(_ => Callback.empty)
                          ),
                          <.label("Times", ^.htmlFor := "repeat-n-times-value")
                        )
                      ).when(e.repeatPeriod)
                    )
                  }
                  .when(e.remainOpenFor)
              )
            },
          Button(
            size = Button.Size.Small,
            disabled = dbActive.value.value,
            onClick = addNewRow(dbActive.setState, props.windows)
          ).compact
            .small(Icons.ThinPlus)
        )
      }
