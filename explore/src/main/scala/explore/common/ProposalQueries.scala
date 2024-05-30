// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import explore.model.Proposal
import explore.model.ProposalType
import lucuma.core.enums.ToOActivation
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import lucuma.schemas.ObservationDB.Types.ProposalTypeInput
import lucuma.schemas.ObservationDB.Types.QueueInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput

trait ProposalQueries:
  private def toOAUpdater(f: Input[ToOActivation] => Input[ToOActivation]) =
    ProposalTypeInput.demoScience.assign.andThen(DemoScienceInput.toOActivation).modify(f) >>>
      ProposalTypeInput.directorsTime.assign.andThen(DirectorsTimeInput.toOActivation).modify(f) >>>
      ProposalTypeInput.fastTurnaround.assign
        .andThen(FastTurnaroundInput.toOActivation)
        .modify(f) >>>
      ProposalTypeInput.largeProgram.assign.andThen(LargeProgramInput.toOActivation).modify(f) >>>
      ProposalTypeInput.queue.assign.andThen(QueueInput.toOActivation).modify(f) >>>
      ProposalTypeInput.systemVerification.assign
        .andThen(SystemVerificationInput.toOActivation)
        .modify(f)

  def modifyToOActivation(
    f: Input[ToOActivation] => Input[ToOActivation]
  ): ProposalPropertiesInput => ProposalPropertiesInput =
    ProposalPropertiesInput.`type`.modify(_.map(toOAUpdater(f)))

  extension (proposalType: ProposalType)
    def toInput: ProposalTypeInput =
      proposalType match
        case ProposalType.DemoScience(_, toOActivation, minPercentTime)                   =>
          ProposalTypeInput(demoScience =
            DemoScienceInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.DirectorsTime(_, toOActivation, minPercentTime)                 =>
          ProposalTypeInput(directorsTime =
            DirectorsTimeInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.FastTurnaround(_, toOActivation, minPercentTime, piAffiliation) =>
          ProposalTypeInput(fastTurnaround =
            FastTurnaroundInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              piAffiliation = piAffiliation.orUnassign
            ).assign
          )
        case ProposalType.LargeProgram(_,
                                       toOActivation,
                                       minPercentTime,
                                       minPercentTotalTime,
                                       totalTime
            ) =>
          ProposalTypeInput(largeProgram =
            LargeProgramInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              minPercentTotalTime = minPercentTotalTime.assign
              // totalTime = totalTime.toInput.assign
            ).assign
          )
        case ProposalType.Classical(_, minPercentTime, partnerSplits)                     =>
          ProposalTypeInput(classical =
            ClassicalInput(
              minPercentTime = minPercentTime.assign
              // partnerSplits = partnerSplits.map(_.toInput).assign
            ).assign
          )
        case ProposalType.Queue(_, toOActivation, minPercentTime, partnerSplits)          =>
          ProposalTypeInput(queue =
            QueueInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.SystemVerification(_, toOActivation, minPercentTime)            =>
          ProposalTypeInput(systemVerification =
            SystemVerificationInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.PoorWeather(scienceSubtype)                                     =>
          ProposalTypeInput(poorWeather = PoorWeatherInput().assign)

  extension (proposal: Proposal)
    def toInput: ProposalPropertiesInput =
      ProposalPropertiesInput(
        callId = proposal.cfpId.orUnassign,
        title = proposal.title.orUnassign,
        category = proposal.category.orUnassign,
        `abstract` = proposal.abstrakt.orUnassign,
        `type` = proposal.proposalType.map(_.toInput).orUnassign
        // The API allows the partner splits to be missing, but not empty. We only use this on
        // create, and it results in an empty partner splits in the response.
        // partnerSplits =
        //   if (proposal.partnerSplits.isEmpty) Input.unassign
        //   else
        //     proposal.partnerSplits.toList.map { case (par, pct) =>
        //       PartnerSplitInput(par, pct)
        //     }.assign
      )

object ProposalQueries extends ProposalQueries
