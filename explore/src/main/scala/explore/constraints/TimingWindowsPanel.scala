// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import eu.timepit.refined.cats.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.TimingWindowQueries
import explore.common.TimingWindowQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.Constants.DurationLongFormatter
import explore.model.TimingWindow
import explore.model.TimingWindowRepeat
import explore.model.TimingWindowRepeatPeriod
import explore.model.TimingWindowType
import explore.model.formats.*
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.given
import lucuma.core.syntax.display.given
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.typed.tanstackTableCore.buildLibTypesMod.Column
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.render.*
import lucuma.ui.syntax.util.*
import lucuma.ui.table.PrimeAutoHeightVirtualizedTable
import lucuma.ui.table.PrimeTable
import lucuma.ui.table.*
import lucuma.ui.utils.Render
import monocle.Optional
import monocle.Traversal
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

  private given Render[TimingWindowRepeatPeriod] = Render.by {
    case TimingWindowRepeatPeriod(period, None)                     =>
      React.Fragment(
        "repeat ",
        <.b("forever"),
        " with a period of ",
        <.b(durationHMS.reverseGet(period))
      )
    case TimingWindowRepeatPeriod(period, Some(n)) if n.value === 1 =>
      React.Fragment(s"repeat ", <.b("once"), " after ", <.b(durationHMS.reverseGet(period)))
    case TimingWindowRepeatPeriod(period, Some(n))                  =>
      React.Fragment(
        "repeat ",
        <.b(n.value, " times"),
        " with a period of ",
        <.b(durationHMS.reverseGet(period))
      )
  }

  private given Render[TimingWindowRepeat] = Render.by {
    case TimingWindowRepeat(period, None)         =>
      React.Fragment("for ", <.b(DurationLongFormatter(period.toDuration)))
    case TimingWindowRepeat(period, Some(repeat)) =>
      React.Fragment(
        "for ",
        <.b(DurationLongFormatter(period.toDuration)),
        ", ",
        repeat.renderVdom
      )
  }

  private given Render[TimingWindowType] = Render.by(twt =>
    <.span(twt match
      case TimingWindowType.Include => ExploreStyles.TimingWindowInclude
      case TimingWindowType.Exclude => ExploreStyles.TimingWindowExclude
    )(twt.shortName)
  )

  private given Render[TimingWindow] = Render.by {
    case tw @ TimingWindow(_, windowType, from, Some(Right(repeat))) =>
      React.Fragment(windowType.renderVdom, " ", <.b(formatZDT(from)), " ", repeat.renderVdom)
    case tw @ TimingWindow(_, windowType, from, Some(Left(closeOn))) =>
      React.Fragment(
        windowType.renderVdom,
        " ",
        <.b(formatZDT(from)),
        " through ",
        <.b(formatZDT(closeOn))
      )
    case tw @ TimingWindow(_, windowType, from, None)                =>
      React.Fragment(windowType.renderVdom, " ", <.b(formatZDT(from)), " forever")
  }

  private val DeleteColWidth = 20
  private val WindowColId    = "Timing window"
  private val DeleteColId    = "Delete"

  private def deleteRow(
    dbActive: TimingWindowOperating => Callback,
    id:       Int,
    windows:  View[List[TimingWindow]]
  )(using FetchClient[IO, ?, UserPreferencesDB]): Callback =
    dbActive(TimingWindowOperating.Operating) *>
      DeleteTimingWindow[IO]
        .execute(id.assign)
        .flatMap(_ => windows.mod(_.filterNot(_.id === id)).to[IO])
        .guarantee(dbActive(TimingWindowOperating.Idle).to[IO])
        .runAsyncAndForget

  private def addNewRow(
    dbActive: TimingWindowOperating => Callback,
    windows:  View[List[TimingWindow]]
  )(using FetchClient[IO, ?, UserPreferencesDB]): Callback =
    dbActive(TimingWindowOperating.Operating) *> CallbackTo.now
      .flatMap { i =>
        val from =
          ZonedDateTime.ofInstant(i, ZoneOffset.UTC).withSecond(0).withNano(0)

        InsertTimingWindow[IO]
          .execute(from.assign)
          .flatMap(id =>
            id.insertTmpTimingWindowsOne
              .map(_.id)
              .map(id =>
                windows
                  .mod(l =>
                    (TimingWindow
                      .forever(id, TimingWindowType.Include, from) :: l.reverse).reverse
                  )
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
              identity,
              size = resize.width.map(z => (z - DeleteColWidth).toPx).getOrElse(400.toPx)
            ).setCell(_.value.renderVdom),
            ColDef(
              ColumnId(DeleteColId),
              _.id,
              size = DeleteColWidth.toPx
            ).setCell(c =>
              Button(
                text = true,
                onClickE = e =>
                  e.stopPropagationCB *> deleteRow(
                    operating.setState,
                    c.value,
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
                TimingWindowQueries.updateTimingWindow[IO](tw).runAsyncAndForget
              case None     => Callback.empty
            }

        val selectedType         = selectedTW.zoom(TimingWindow.windowType)
        val selectedfrom         = selectedTW.zoom(TimingWindow.from)
        val selectedThrough      = selectedTW.zoom(TimingWindow.through)
        val selectedFiniteSpan   = selectedTW.zoom(TimingWindow.finiteSpan)
        val selectedRepeatPeriod = selectedTW.zoom(TimingWindow.repeatPeriod)
        val selectedRepeatNTimes = selectedTW.zoom(TimingWindow.repeatFrequency)

        // TODO Should we move this to lucuma-ui?
        val HMPartialRegEx                  = "\\d*(:\\d{0,2})?".r
        val hmChangeAuditor: ChangeAuditor  = ChangeAuditor.accept.allow(HMPartialRegEx.matches)
        val HMSPartialRegEx                 = "\\d*(:\\d{0,2}(:\\d{0,2})?)?".r
        val hmsChangeAuditor: ChangeAuditor = ChangeAuditor.accept.allow(HMSPartialRegEx.matches)

        def renderTypeRadio(twt: TimingWindowType, id: String): VdomNode =
          <.span(
            RadioButton(
              twt,
              id = id,
              checked = selectedType.get.contains_(twt),
              onChange = (v, checked) => selectedType.set(v).when_(checked)
            ),
            <.label(^.htmlFor := id, twt.renderVdom)
          )

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
                <.span(ExploreStyles.TimingWindowEditorHeader)(
                  <.span(ExploreStyles.TimingWindowTypeEditor)(
                    renderTypeRadio(TimingWindowType.Include, "include-option"),
                    renderTypeRadio(TimingWindowType.Exclude, "exclude-option")
                  ),
                  <.span(ExploreStyles.TimingWindowFromEditor)(
                    " from",
                    Datepicker(onChange =
                      (newValue, _) =>
                        newValue.fromDatePickerToZDTOpt.foldMap { i =>
                          selectedfrom.set(i.withSecond(0).withNano(0))
                        }
                    )
                      .showTimeInput(true)
                      .selected(e.from.toDatePickerJsDate)
                      .dateFormat("yyyy-MM-dd HH:mm"),
                    " UTC"
                  )
                ),
                <.div(ExploreStyles.TimingWindowEditorBody)(
                  <.div(
                    RadioButton(
                      "forever",
                      id = "forever-option",
                      checked = e.forever,
                      onChange = (_, checked) => selectedTW.mod(_.toForever).when_(checked)
                    ),
                    <.label("Forever", ^.htmlFor := "forever-option")
                  ),
                  <.div(
                    RadioButton(
                      "close-on",
                      id = "close-on-option",
                      checked = e.through,
                      onChange = (_, checked) =>
                        CallbackTo.now
                          .flatMap(i =>
                            selectedTW.mod(_.through(ZonedDateTime.ofInstant(i, ZoneOffset.UTC)))
                          )
                          .when_(checked)
                    ),
                    <.label("Through ", ^.htmlFor := "close-on-option"),
                    selectedThrough.asView.map { closeOn =>
                      React.Fragment(
                        Datepicker(onChange =
                          (newValue, _) =>
                            newValue.fromDatePickerToZDTOpt.foldMap { i =>
                              selectedThrough.set(i.withSecond(0).withNano(0))
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
                      checked = e.finiteSpan,
                      onChange = (_, checked) =>
                        selectedTW
                          .mod(
                            _.toFiniteSpan(TimeSpan.unsafeFromDuration(Duration.ofDays(2)))
                          )
                          .when_(checked)
                    ),
                    <.label("For", ^.htmlFor := "remain-open"),
                    selectedFiniteSpan.asView.map { remainOpen =>
                      FormInputTextView(
                        id = "remain-duration".refined,
                        value = remainOpen,
                        validFormat = durationHM,
                        changeAuditor = hmChangeAuditor
                      )
                    },
                    " hours"
                  ),
                  selectedFiniteSpan.asView
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
                            changeAuditor = hmsChangeAuditor,
                            disabled = !e.repeatPeriod
                          ),
                          " hours"
                        ),
                        <.div(
                          ExploreStyles.TimingWindowRepeatEditorAlternatives,
                          <.div(
                            RadioButton(
                              "repeat-forever",
                              id = "repeat-forever-option",
                              checked = e.repeatForever,
                              onChange = (_, checked) =>
                                selectedTW.mod(_.toRepeatPeriodForever).when_(checked)
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
                            <.label("times", ^.htmlFor := "repeat-n-times-value")
                          )
                        ).when(e.repeatPeriod)
                      )
                    }
                    .when(e.finiteSpan)
                )
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
