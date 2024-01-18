// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.syntax.all.toHoursMinutes
import explore.proposal.ProposalInfo
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps

case class ProgramDetailsTile(optProposal: Option[ProposalInfo])
    extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:

  private type Props = ProgramDetailsTile

  def component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>

      val timeAccounting = for {
        propInfo    <- props.optProposal
        minTime     <- propInfo.minExecutionTime
        maxTime     <- propInfo.maxExecutionTime
        used         = propInfo.timeCharge.program
        remain       = TimeSpan.Zero // TODO
        isSingleTime = minTime == maxTime
      } yield table(
        headers = Seq("Time accounting"),
        rows = (if (isSingleTime) Seq(Seq[TagMod]("Planned", minTime.toHoursMinutes))
                else
                  Seq(
                    Seq[TagMod]("Min Time", minTime.toHoursMinutes),
                    Seq[TagMod]("Max Time", maxTime.toHoursMinutes)
                  )) ++
          Seq(Seq[TagMod]("Used", used.toHoursMinutes)),
        footer = Seq(Seq[TagMod]("Remain", remain.toHoursMinutes))
      )

      <.div(
        ExploreStyles.ProgramDetailsTile,
        <.div(
          timeAccounting
        )
      )
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
