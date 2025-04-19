// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.model.Proposal
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ProposalReference
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message

import scala.concurrent.duration.*

case class SubmittedProposalMessage(
  proposalReference: Option[ProposalReference],
  deadline:          Option[Timestamp]
) extends ReactFnProps(SubmittedProposalMessage.component):
  private val proposalReferenceStr: String =
    proposalReference.map(pr => s" as ${pr.label}").orEmpty

  private val deadlineStr: String =
    deadline
      .map(d => s" until the proposal deadline at ${Proposal.deadlineString(d)}")
      .orEmpty

object SubmittedProposalMessage:
  private type Props = SubmittedProposalMessage

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStreamOnMount:
        Stream
          .fixedRateStartImmediately[IO](1.second)
          .evalMap: _ =>
            IO.realTime.map: finiteDuration =>
              Timestamp.ofEpochMilli(finiteDuration.toMillis)
      .render: (props, nowPot) =>
        val retractStr: String = nowPot.toOption.flatten
          .filter(now => props.deadline.exists(_ > now))
          .as(s" and may be retracted${props.deadlineStr}")
          .orEmpty

        Message(text =
          s"The proposal has been submitted${props.proposalReferenceStr}${retractStr}."
        )
