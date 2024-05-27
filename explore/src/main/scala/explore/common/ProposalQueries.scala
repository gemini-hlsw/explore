// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import explore.model.Proposal
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import clue.data.Input
import clue.data.syntax.*

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
  extension (proposal: Proposal)
    def toInput: ProposalPropertiesInput = {
      println(proposal.abstrakt)
      ProposalPropertiesInput(
        callId = proposal.cfpId.orUnassign,
        title = proposal.title.orUnassign,
        category = proposal.category.orUnassign,
        `abstract` = proposal.abstrakt.orUnassign
        // The API allows the partner splits to be missing, but not empty. We only use this on
        // create, and it results in an empty partner splits in the response.
        // partnerSplits =
        //   if (proposal.partnerSplits.isEmpty) Input.unassign
        //   else
        //     proposal.partnerSplits.toList.map { case (par, pct) =>
        //       PartnerSplitInput(par, pct)
        //     }.assign
      )
    }

object ProposalQueries extends ProposalQueries
