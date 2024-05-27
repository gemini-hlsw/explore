// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import explore.model.Proposal
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import clue.data.Input
import clue.data.syntax.*
import explore.model.ProposalType
import lucuma.schemas.ObservationDB.Types.ProposalTypeInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.odb.input.given
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput
import lucuma.schemas.ObservationDB.Types.QueueInput

// extension (p: ProposalClass)
//   def toInput: ProposalClassInput = p match
//     case DemoScience(minPercentTime)                                  =>
//       ProposalClassInput(demoScience =
//         DemoScienceInput(minPercentTime = minPercentTime.assign).assign
//       )
//     case Exchange(minPercentTime)                                     =>
//       ProposalClassInput(exchange = ExchangeInput(minPercentTime = minPercentTime.assign).assign)
//     case LargeProgram(minPercentTime, minPercentTotalTime, totalTime) =>
//       ProposalClassInput(largeProgram =
//         LargeProgramInput(
//           minPercentTime = minPercentTime.assign,
//           minPercentTotalTime = minPercentTotalTime.assign,
//           totalTime = totalTime.toInput.assign
//         ).assign
//       )
//     case Queue(minPercentTime)                                        =>
//       ProposalClassInput(queue = QueueInput(minPercentTime = minPercentTime.assign).assign)
//     case FastTurnaround(minPercentTime)                               =>
//       ProposalClassInput(fastTurnaround =
//         FastTurnaroundInput(minPercentTime = minPercentTime.assign).assign
//       )
//     case DirectorsTime(minPercentTime)                                =>
//       ProposalClassInput(directorsTime =
//         DirectorsTimeInput(minPercentTime = minPercentTime.assign).assign
//       )
//     case Intensive(minPercentTime, minPercentTotalTime, totalTime)    =>
//       ProposalClassInput(intensive =
//         IntensiveInput(
//           minPercentTime = minPercentTime.assign,
//           minPercentTotalTime = minPercentTotalTime.assign,
//           totalTime = totalTime.toInput.assign
//         ).assign
//       )
//     case SystemVerification(minPercentTime)                           =>
//       ProposalClassInput(systemVerification =
//         SystemVerificationInput(minPercentTime = minPercentTime.assign).assign
//       )
//     case Classical(minPercentTime)                                    =>
//       ProposalClassInput(classical = ClassicalInput(minPercentTime = minPercentTime.assign).assign)
//     case PoorWeather(minPercentTime)                                  =>
//       ProposalClassInput(poorWeather =
//         PoorWeatherInput(minPercentTime = minPercentTime.assign).assign
//       )

trait ProposalQueries:
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
