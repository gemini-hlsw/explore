// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.Proposal
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message

import java.time.Instant
import scala.concurrent.duration.*

case class CallDeadline(deadline: Timestamp) extends ReactFnProps(CallDeadline.component)

object CallDeadline:
  private type Props = CallDeadline

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStreamOnMount:
        Stream.eval(IO(Instant.now())) ++
          Stream
            .awakeDelay[IO](1.seconds)
            .flatMap(_ => Stream.eval(IO(Instant.now())))
      .render: (p, n) =>
        n.toOption.map: n =>
          val (deadlineStr, left) = Proposal.deadlineAndTimeLeft(n, p.deadline)
          val text                = left.fold(deadlineStr)(l => s"$deadlineStr [$l]")

          <.span(ExploreStyles.ProposalDeadline)(
            Message(
              text = s"Deadline: $text",
              severity = Message.Severity.Info
            )
          )
