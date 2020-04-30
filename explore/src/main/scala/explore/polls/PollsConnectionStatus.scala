// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.polls

import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.elements.button._
import react.semanticui.elements.segment.Segment
import react.semanticui.colors._
import react.semanticui.SemanticColor
import react.common._
import crystal.react.implicits._
import clue.StreamingClientStatus

object PollsConnectionStatus {
  type Props = AppContextIO

  private def statusColor(s: StreamingClientStatus): SemanticColor =
    s match {
      case StreamingClientStatus.Open    => Green
      case StreamingClientStatus.Closing => Orange
      case StreamingClientStatus.Closed  => Red
      case _                             => Yellow
    }

  val component =
    ScalaComponent
      .builder[Props]("PollsConnectionStatus")
      .render_P { ctx =>
        <.div(
          ctx.clients.pollConnectionStatus(status =>
            Segment(color = statusColor(status), inverted = true)(s"Poll connection is: $status")
          ),
          Button(
            negative = true,
            onClick = ctx.clients.polls.close().runInCB
          )("Close Connection")
        )
      }
      .build

  def apply()(implicit props: Props) = component(props)
}
