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
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.TimeSpanView
import lucuma.ui.syntax.all.*

case class ProgramDetailsTile(
  programTimes: Pot[ProgramTimes]
) extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:

  private type Props = ProgramDetailsTile

  val component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>
      props.programTimes.renderPot { programTimes =>
        val timeAccounting = for {
          minTime     <- programTimes.timeEstimateRange.map(_.minimum.value)
          maxTime     <- programTimes.timeEstimateRange.map(_.maximum.value)
          used         = programTimes.timeCharge.value
          remain       = TimeSpan.Zero // TODO
          isSingleTime = minTime == maxTime
        } yield table(
          headers = Seq("Time accounting"),
          rows = (if (isSingleTime) Seq(Seq[TagMod]("Planned", TimeSpanView(minTime)))
                  else
                    Seq(
                      Seq[TagMod]("Min Time", TimeSpanView(minTime)),
                      Seq[TagMod]("Max Time", TimeSpanView(maxTime))
                    )) ++
            Seq(Seq[TagMod]("Used", TimeSpanView(used))),
          footer = Seq(Seq[TagMod]("Remain", TimeSpanView(remain)))
        )

        <.div(
          ExploreStyles.ProgramDetailsTile,
          <.div(
            timeAccounting
          )
        )
      }
    }

  private def table(
    headers: Seq[String],
    rows:    Seq[Seq[TagMod]],
    footer:  Seq[Seq[TagMod]]
  ): VdomNode =
    <.table(ExploreStyles.ProgramTabTable)(
      headers.nonEmpty
        .guard[Option]
        .as(
          <.thead(
            <.tr(
              headers.toTagMod(h =>
                <.th(^.colSpan := rows.headOption
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
            footer.toTagMod(r => <.tr(r.toTagMod(c => <.td(c))))
          )
        )
    )
