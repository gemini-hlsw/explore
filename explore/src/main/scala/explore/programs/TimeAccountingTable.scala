// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import crystal.Pot
import explore.components.ui.ExploreStyles
import explore.model.ProgramTimes
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.TimeSpanView
import lucuma.ui.syntax.all.*

case class TimeAccountingTable(programTimes: Pot[ProgramTimes])
    extends ReactFnProps(TimeAccountingTable.component)

object TimeAccountingTable:
  private type Props = TimeAccountingTable

  private def table(
    headers: List[String],
    rows:    List[List[TagMod]],
    footer:  List[List[TagMod]]
  ): VdomNode =
    // TODO The "pl-react-table" is just used to unify styles (time award table needs it for specificity).
    // We will probably change this table to a react-table once we add columns for bands.
    // See https://app.shortcut.com/lucuma/story/2947/display-time-award-on-the-program-tab
    <.table(ExploreStyles.ProgramTabTable |+| Css("pl-react-table"))(
      headers.nonEmpty
        .guard[Option]
        .as(
          <.thead(
            <.tr(
              headers.toTagMod(h =>
                <.th(
                  ^.colSpan := rows.headOption
                    .filter(_ => headers.length == 1)
                    .map(_.length)
                    .getOrElse(1),
                  h
                )
              )
            )
          )
        ),
      rows.nonEmpty
        .guard[Option]
        .as(
          <.tbody(
            rows.toTagMod(r => <.tr(r.toTagMod(c => <.td(c))))
          )
        ),
      footer.nonEmpty
        .guard[Option]
        .as(
          <.tfoot(
            footer.toTagMod(r => <.tr(r.toTagMod(c => <.th(c))))
          )
        )
    )

  private val component = ScalaFnComponent[Props]: props =>
    props.programTimes.renderPot: programTimes =>
      for
        minTime     <- programTimes.timeEstimateRange.map(_.minimum.value)
        maxTime     <- programTimes.timeEstimateRange.map(_.maximum.value)
        used         = programTimes.fullProgramTime
        remain       = TimeSpan.Zero // TODO
        isSingleTime = minTime == maxTime
      yield table(
        headers = List("Time Accounting"),
        rows = (if (isSingleTime) List(List[TagMod]("Planned", TimeSpanView(minTime)))
                else
                  List(
                    List[TagMod]("Min Time", TimeSpanView(minTime)),
                    List[TagMod]("Max Time", TimeSpanView(maxTime))
                  )) ++
          List(List[TagMod]("Used", TimeSpanView(used))),
        footer = List(List[TagMod]("Remain", TimeSpanView(remain)))
      )
