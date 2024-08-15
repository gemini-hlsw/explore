// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.timingwindows

import cats.Order.given
import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.Constants.BadTimingWindow
import explore.model.ObsTabTilesIds
import explore.model.formats.*
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.render.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.model.TimingWindowRepeat
import lucuma.core.syntax.display.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.Datepicker
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.render.*
import lucuma.ui.table.*
import lucuma.ui.table.PrimeTable
import lucuma.ui.utils.Render
import monocle.Iso
import monocle.function.Index
import monocle.function.Index.*

import java.time.Duration
import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import explore.model.enums.TileSizeState

case class TimingWindowsTileState(
  setRowSelection: RowSelection => Callback = _ => Callback.empty
)

case class TimingWindowsBody(
  windows:  View[List[TimingWindow]],
  readOnly: Boolean
)(val state: View[TimingWindowsTileState])
    extends ReactFnProps(TimingWindowsBody.component)

object TimingWindowsTile:
  def timingWindowsPanel(
    timingWindows: View[List[TimingWindow]],
    readOnly:      Boolean
  ) =
    val base  = "Scheduling Windows"
    val title =
      if (timingWindows.get.isEmpty) base else s"$base (${timingWindows.get.length})"
    Tile(ObsTabTilesIds.TimingWindowsId.id, TimingWindowsTileState(), title)(
      TimingWindowsBody(timingWindows, readOnly),
      TimingWindowsTitle(timingWindows, readOnly)
    )

object TimingWindowsBody:
  private type Props = TimingWindowsBody

  private val ColDef = ColumnDef[(TimingWindow, Int)]

  private given Render[TimingWindowInclusion] = Render.by(twt =>
    <.span(twt match
      case TimingWindowInclusion.Include => ExploreStyles.TimingWindowInclude
      case TimingWindowInclusion.Exclude => ExploreStyles.TimingWindowExclude
    )(twt.shortName)
  )

  private given Render[TimingWindow] = Render.by {
    case tw @ TimingWindow(inclusion, start, Some(TimingWindowEnd.At(endAt)))           =>
      React.Fragment(
        inclusion.renderVdom,
        " ",
        <.b(start.formatUtcWithZone),
        " through ",
        <.b(endAt.formatUtcWithZone)
      )
    case tw @ TimingWindow(inclusion, start, Some(after @ TimingWindowEnd.After(_, _))) =>
      React.Fragment(inclusion.renderVdom, " ", <.b(start.formatUtcWithZone), " ", after.renderVdom)
    case tw @ TimingWindow(inclusion, start, None)                                      =>
      React.Fragment(inclusion.renderVdom, " ", <.b(start.formatUtcWithZone), " forever")
  }

  private val DeleteColWidth: Int   = 20
  private val WindowColId: ColumnId = ColumnId("TimingWindow")
  private val DeleteColId: ColumnId = ColumnId("Delete")

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useResizeDetector()
      .useMemoBy((_, _) => ()): (props, _) => // cols
        _ =>
          List(
            ColDef(
              WindowColId,
              _._1,
              size = 400.toPx
            ).setCell: cell =>
              <.span(
                cell.value.renderVdom,
                <.span(Icons.ErrorIcon).withTooltip(BadTimingWindow).unless(cell.value.isValid)
              ),
            ColDef(
              DeleteColId,
              _._2,
              size = DeleteColWidth.toPx
            ).setCell(c =>
              Button(
                text = true,
                onClickE = e =>
                  e.stopPropagationCB >>
                    props.windows.mod(tws => tws.take(c.value) ++ tws.drop(c.value + 1))
              ).compact.small(Icons.Trash)
            )
          )
      .useMemoBy((props, _, _) => props.windows.get): // rows
        (_, _, _) => _.zipWithIndex.sorted
      .useReactTableBy: (props, resize, cols, rows) =>
        TableOptions(
          cols,
          rows,
          enableRowSelection = true,
          getRowId = (row, _, _) => RowId(row._2.toString),
          state = PartialTableState(
            columnSizing = ColumnSizing(
              WindowColId -> resize.width.map(w => (w - DeleteColWidth).toPx).getOrElse(400.toPx)
            ),
            columnVisibility = ColumnVisibility(
              DeleteColId -> Visibility.fromVisible(!props.readOnly)
            )
          )
        )
      .useEffectOnMountBy((p, _, _, _, table) =>
        val cb = (a: RowSelection) => table.setRowSelection(a)
        p.state.set(TimingWindowsTileState(cb))
      )
      .render: (props, resize, dbActive, rows, table) =>
        val pos = table.getSelectedRowModel().rows.headOption.map(_.original._2)

        val selectedTW: Option[View[TimingWindow]] =
          pos
            .filterNot(_ => props.readOnly)
            .flatMap(p =>
              props.windows
                .zoom(Index.index[List[TimingWindow], Int, TimingWindow](p))
                .asView
            )

        // TODO Should we move this to lucuma-ui?
        val HMPartialRegEx                  = "\\d*(:\\d{0,2})?".r
        val hmChangeAuditor: ChangeAuditor  = ChangeAuditor.accept.allow(HMPartialRegEx.matches)
        val HMSPartialRegEx                 = "\\d*(:\\d{0,2}(:\\d{0,2})?)?".r
        val hmsChangeAuditor: ChangeAuditor = ChangeAuditor.accept.allow(HMSPartialRegEx.matches)

        <.div(ExploreStyles.TimingWindowsBody)(
          <.div.withRef(resize.ref)(ExploreStyles.TimingWindowsTable)(
            PrimeTable(
              table,
              striped = true,
              compact = Compact.Very,
              headerMod = ExploreStyles.TimingWindowsHeader,
              tableMod =
                ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable |+| ExploreStyles.TimingWindowsTable,
              rowMod = row =>
                TagMod(
                  ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                  ^.onClick --> (table.toggleAllRowsSelected(false) >> row.toggleSelected())
                ),
              cellMod = _.column.id match
                case DeleteColId => ^.textAlign.right
                case _           => TagMod.empty
              ,
              // If cmd is pressed add to the selection
              emptyMessage = <.div(ExploreStyles.ExploreTableEmpty, "No scheduling windows defined")
            )
          ),
          selectedTW.map { tw =>
            val selectedInclusion: View[TimingWindowInclusion]   = tw.zoom(TimingWindow.inclusion)
            val selectedStart: View[Timestamp]                   = tw.zoom(TimingWindow.start)
            val selectedEnd: View[Option[TimingWindowEnd]]       = tw.zoom(TimingWindow.end)
            val selectedEndAt: ViewOpt[Timestamp]                =
              selectedEnd
                .zoom(Iso.id[Option[TimingWindowEnd]].some)
                .zoom(TimingWindowEnd.at)
                .zoom(TimingWindowEnd.At.instant)
            val selectedEndAfter: ViewOpt[TimingWindowEnd.After] =
              selectedEnd
                .zoom(Iso.id[Option[TimingWindowEnd]].some)
                .zoom(TimingWindowEnd.after)

            def renderInclusionRadio(twt: TimingWindowInclusion, id: String): VdomNode =
              <.span(
                RadioButton(
                  twt,
                  id = id,
                  checked = selectedInclusion.get === twt,
                  onChange = (v, checked) => selectedInclusion.set(v).when_(checked)
                ),
                <.label(^.htmlFor := id, twt.renderVdom)
              )

            <.div(ExploreStyles.TimingWindowEditor)(
              <.span(ExploreStyles.TimingWindowEditorHeader)(
                <.span(ExploreStyles.TimingWindowInclusionEditor)(
                  renderInclusionRadio(TimingWindowInclusion.Include, "include-option"),
                  renderInclusionRadio(TimingWindowInclusion.Exclude, "exclude-option")
                ),
                <.div(ExploreStyles.TimingWindowFromEditor)(
                  <.span(" from"),
                  Datepicker(onChange =
                    (newValue, _) =>
                      newValue.fromDatePickerToZDTOpt.foldMap { zdt =>
                        selectedStart.set(
                          Timestamp.unsafeFromInstantTruncated(
                            zdt.withSecond(0).withNano(0).toInstant
                          )
                        )
                      }
                  )
                    .showTimeInput(true)
                    .selected(selectedStart.get.toInstant.toDatePickerJsDate)
                    .dateFormat("yyyy-MM-dd HH:mm")
                    .maxDate(
                      selectedEnd.get
                        .flatMap(TimingWindowEnd.at.getOption)
                        .map(_.instant.toInstant.toDatePickerJsDate)
                        .orNull
                    ),
                  <.span(" UTC "),
                  <.span(Icons.ErrorIcon)
                    .withTooltip("Check start date is before the end")
                    .unless(tw.get.isValid)
                )
              ),
              <.div(ExploreStyles.TimingWindowEditorBody)(
                <.div(
                  RadioButton(
                    "forever",
                    id = "forever-option",
                    checked = selectedEnd.get.isEmpty,
                    onChange = (_, checked) => selectedEnd.set(none).when_(checked)
                  ),
                  <.label("Forever", ^.htmlFor := "forever-option")
                ),
                <.div(
                  ExploreStyles.TimingWindowThroughEditor,
                  RadioButton(
                    "through",
                    id = "through-option",
                    checked = selectedEndAt.get.isDefined,
                    onChange = (_, checked) =>
                      selectedEnd
                        .set(
                          TimingWindowEnd
                            .At(
                              Timestamp.unsafeFromInstantTruncated(
                                ZonedDateTime
                                  .ofInstant(selectedStart.get.toInstant, ZoneOffset.UTC)
                                  .plusHours(1)
                                  .toInstant
                              )
                            )
                            .some
                        )
                        .when_(checked)
                  ),
                  <.label("Through ", ^.htmlFor := "through-option"),
                  selectedEndAt.mapValue(endAt =>
                    React.Fragment(
                      Datepicker(onChange =
                        (newValue, _) =>
                          newValue.fromDatePickerToZDTOpt.foldMap(zdt =>
                            endAt.set(
                              Timestamp.unsafeFromInstantTruncated(
                                zdt.withSecond(0).withNano(0).toInstant
                              )
                            )
                          )
                      )
                        .showTimeInput(true)
                        .selected(endAt.get.toInstant.toDatePickerJsDate)
                        .dateFormat("yyyy-MM-dd HH:mm")
                        .minDate(selectedStart.get.toInstant.toDatePickerJsDate),
                      <.span(" UTC "),
                      if (tw.get.isValid) EmptyVdom
                      else
                        <.span(Icons.ErrorIcon)
                          .withTooltip("Check start date is before the end")
                    )
                  )
                ),
                <.div(ExploreStyles.TimingWindowEndAfter)(
                  RadioButton(
                    "for",
                    id = "for",
                    checked = selectedEndAfter.get.isDefined,
                    onChange = (_, checked) =>
                      selectedEnd
                        .set(
                          TimingWindowEnd
                            .After(
                              duration = TimeSpan.unsafeFromDuration(Duration.ofDays(2)),
                              repeat = none
                            )
                            .some
                        )
                        .when_(checked)
                  ),
                  <.label("For", ^.htmlFor := "for"),
                  selectedEndAfter.mapValue { endAfter =>
                    React.Fragment(
                      FormInputTextView(
                        id = "for-duration".refined,
                        value = endAfter.zoom(TimingWindowEnd.After.duration),
                        validFormat = durationHM,
                        changeAuditor = hmChangeAuditor
                      ),
                      " hours"
                    )
                  }
                ),
                selectedEndAfter.mapValue { endAfter =>
                  val selectedRepeat: View[Option[TimingWindowRepeat]] =
                    endAfter.zoom(TimingWindowEnd.After.repeat)
                  val selectedRepeatOpt: ViewOpt[TimingWindowRepeat]   =
                    selectedRepeat.zoom(Iso.id[Option[TimingWindowRepeat]].some)
                  val selectedRepeatPeriod: ViewOpt[TimeSpan]          =
                    selectedRepeatOpt.zoom(TimingWindowRepeat.period)

                  <.div(
                    <.div(ExploreStyles.TimingWindowRepeatEditor)(
                      <.div(LucumaPrimeStyles.CheckboxWithLabel)(
                        Checkbox(
                          id = "repeat-with-period",
                          checked = selectedRepeat.get.isDefined,
                          onChange = checked =>
                            selectedRepeat
                              .set(
                                TimingWindowRepeat(
                                  period = TimeSpan
                                    .unsafeFromDuration(
                                      endAfter.get.duration.toDuration.plusHours(12)
                                    ),
                                  times = none
                                ).some
                              )
                              .when_(checked) >>
                              selectedRepeat.set(none).unless_(checked)
                        ),
                        <.label("Repeat with a period of", ^.htmlFor := "repeat-with-period")
                      ),
                      FormInputTextView(
                        id = "repat-period".refined,
                        value = selectedRepeatPeriod,
                        validFormat = durationHMS,
                        changeAuditor = hmsChangeAuditor,
                        disabled = selectedRepeat.get.isEmpty
                      ),
                      " hours"
                    ),
                    selectedRepeatOpt.mapValue { repeat =>
                      val selectedRepeatTimes: View[Option[PosInt]] =
                        repeat.zoom(TimingWindowRepeat.times)

                      <.div(ExploreStyles.TimingWindowRepeatEditorAlternatives)(
                        <.div(
                          RadioButton(
                            "repeat-forever",
                            id = "repeat-forever-option",
                            checked = selectedRepeatTimes.get.isEmpty,
                            onChange = (_, checked) => selectedRepeatTimes.set(none).when_(checked)
                          ),
                          <.label("Forever", ^.htmlFor := "repeat-forever-option")
                        ),
                        <.div(ExploreStyles.TimingWindowRepeatEditorNTimes)(
                          RadioButton(
                            "repeat-n-times",
                            id = "repeat-n-times",
                            checked = selectedRepeatTimes.get.isDefined,
                            onChange = (_, checked) =>
                              selectedRepeatTimes.set(1.refined[Positive].some).when_(checked)
                          ),
                          FormInputTextView(
                            id = "repeat-n-times-value".refined,
                            value = selectedRepeatTimes.zoom(Iso.id[Option[PosInt]].some),
                            validFormat = InputValidSplitEpi.posInt,
                            changeAuditor = ChangeAuditor.posInt,
                            disabled = selectedRepeatTimes.get.isEmpty
                          ),
                          <.label("times", ^.htmlFor := "repeat-n-times-value")
                        )
                      )
                    }
                  )
                }
              )
            )
          }
        )

case class TimingWindowsTitle(
  windows:  View[List[TimingWindow]],
  readOnly: Boolean
)(val state: View[TimingWindowsTileState], val tileSize: TileSizeState)
    extends ReactFnProps(TimingWindowsTitle.component)

object TimingWindowsTitle:
  private type Props = TimingWindowsTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      if (props.readOnly || props.tileSize === TileSizeState.Minimized) EmptyVdom
      else
        Button(
          severity = Button.Severity.Success,
          icon = Icons.New,
          label = "Add",
          onClick = props.windows.mod(
            _ :+ TimingWindow(
              inclusion = TimingWindowInclusion.Include,
              start = Timestamp.unsafeFromInstantTruncated(Instant.now),
              end = none
            )
          ) >> props.state.get.setRowSelection(
            RowSelection(RowId(props.windows.get.size.toString) -> true)
          )
        ).tiny.compact
