// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import explore.components.ui.ExploreStyles
import explore.model.Proposal
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message

case class CallDeadline(now: Timestamp, deadline: Timestamp)
    extends ReactFnProps(CallDeadline.component)

object CallDeadline:
  private type Props = CallDeadline

  private val component =
    ScalaFnComponent[Props]: props =>
      val (deadlineStr, left): (String, Option[String]) =
        Proposal.deadlineAndTimeLeft(props.now, props.deadline)
      val text: String                                  =
        left.fold(deadlineStr)(l => s"$deadlineStr [$l]")
      val severity: Message.Severity                    =
        left.fold(Message.Severity.Error)(_ => Message.Severity.Info)

      <.span(ExploreStyles.ProposalDeadline)(
        Message(
          text = s"Deadline: $text",
          severity = severity
        )
      )
