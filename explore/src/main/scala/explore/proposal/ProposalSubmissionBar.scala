// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.ResponseException
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Proposal
import explore.syntax.ui.*
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Toolbar
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.SetProposalStatusInput
import lucuma.schemas.enums.ProposalStatus
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import org.typelevel.log4cats.Logger
import queries.common.ProposalQueriesGQL.SetProposalStatus

import scala.concurrent.duration.*

case class ProposalSubmissionBar(
  programId:      Program.Id,
  proposalStatus: View[ProposalStatus],
  deadline:       Option[Timestamp],
  callId:         Option[CallForProposals.Id],
  canSubmit:      Boolean
) extends ReactFnProps(ProposalSubmissionBar.component)

object ProposalSubmissionBar:
  private type Props = ProposalSubmissionBar

  private object IsUpdatingStatus extends NewType[Boolean]
  private type IsUpdatingStatus = IsUpdatingStatus.Type

  private def doUpdateStatus(
    programId:              Program.Id,
    isUpdatingStatus:       View[IsUpdatingStatus],
    setLocalProposalStatus: ProposalStatus => IO[Unit],
    setErrorMessage:        Option[String] => IO[Unit]
  )(
    newStatus:              ProposalStatus
  )(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): Callback =
    (for {
      _ <- SetProposalStatus[IO]
             .execute:
               SetProposalStatusInput(programId = programId.assign, status = newStatus)
             .onError:
               case ResponseException(errors, _) =>
                 setErrorMessage(errors.head.message.some)
               case e                            =>
                 setErrorMessage(Some(e.getMessage.toString))
             .void
      _ <- setLocalProposalStatus(newStatus)
    } yield ()).switching(isUpdatingStatus.async, IsUpdatingStatus(_)).runAsync

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsUpdatingStatus(false))
      .useStateView(none[String]) // Submission error message
      .useLayoutEffectWithDepsBy((props, _, _, _) => props.callId): (_, _, _, e) =>
        _ => e.set(none)          // Reset error message on CfP change
      .useStreamOnMount:
        Stream
          .fixedRateStartImmediately[IO](1.second)
          .evalMap: _ =>
            IO.realTime.map: finiteDuration =>
              Timestamp.ofEpochMilli(finiteDuration.toMillis)
      .render: (props, ctx, isUpdatingStatus, errorMessage, nowPot) =>
        import ctx.given

        val updateStatus: ProposalStatus => Callback =
          doUpdateStatus(
            props.programId,
            isUpdatingStatus,
            props.proposalStatus.async.set,
            errorMessage.async.set
          )

        nowPot.toOption.flatten.map: now =>
          val isDueDeadline: Boolean = props.deadline.forall(_ < now)

          Toolbar(left =
            <.div(ExploreStyles.ProposalSubmissionBar)(
              Tag(
                value = props.proposalStatus.get.name,
                severity =
                  if (props.proposalStatus.get === ProposalStatus.Accepted) Tag.Severity.Success
                  else Tag.Severity.Danger
              )
                .when(props.proposalStatus.get > ProposalStatus.Submitted),
              // TODO: Validate proposal before allowing submission
              React
                .Fragment(
                  Button(
                    label = "Submit Proposal",
                    onClick = updateStatus(ProposalStatus.Submitted),
                    disabled = isUpdatingStatus.get.value || props.callId.isEmpty || isDueDeadline
                  ).compact.tiny,
                  props.deadline.map: deadline =>
                    val (deadlineStr, left): (String, Option[String]) =
                      Proposal.deadlineAndTimeLeft(now, deadline)
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
                )
                .when:
                  props.canSubmit && props.proposalStatus.get === ProposalStatus.NotSubmitted
              ,
              Button(
                "Retract Proposal",
                severity = Button.Severity.Warning,
                onClick = updateStatus(ProposalStatus.NotSubmitted),
                disabled = isUpdatingStatus.get.value || isDueDeadline
              ).compact.tiny
                .when:
                  props.canSubmit && props.proposalStatus.get === ProposalStatus.Submitted && !isDueDeadline
              ,
              errorMessage.get
                .map(r => Message(text = r, severity = Message.Severity.Error))
            )
          )
