// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.proposal.ProposalInfo
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps

case class ProgramDetailsTile(optProposal: Option[ProposalInfo])
    extends ReactFnProps(ProgramDetailsTile.component)

object ProgramDetailsTile:

  private type Props = ProgramDetailsTile

  def component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>

      val timeAwards = for {
        propInfo <- props.optProposal
        proposal <- propInfo.optProposal
      } yield table(
        headers = Seq("Time Award", "Band 1"),
        rows = proposal.partnerSplits
          .map((partner, split) =>
            Seq[VdomNode](
              <.span(
                ^.verticalAlign.middle,
                partner.abbreviation,
                <.img(^.src        := PartnerFlags.smallFlag(partner),
                      ^.alt := s"${partner.name}  Flag",
                      ExploreStyles.PartnerSplitFlag,
                      ^.verticalAlign.middle
                )
              ),
              split.value.toString()
            )
          )
          .toSeq
      )

      val timeAccounting = for {
        propInfo      <- props.optProposal
        proposal      <- propInfo.optProposal
        executionTime <- propInfo.executionTime
      } yield table(
        headers = Seq("Time accounting"),
        rows = Seq(
        )
      )

      <.div(
        ExploreStyles.ProgramDetailsTile,
        <.div("col1"),
        <.div("col2")(
          timeAwards,
          timeAccounting
        ),
        <.div("col3")
      )
    }

  private def table(headers: Seq[String], rows: Seq[Seq[TagMod]]): VdomNode =
    <.table(ExploreStyles.ProgramTabTable)(
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
      ),
      <.tbody(
        rows.toTagMod(r => <.tr(r.toTagMod(c => <.td(c))))
      )
    )
