// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.Semester
import lucuma.core.util.Enumerated
import lucuma.schemas.decoders.given
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.cats.given
import io.circe.refined.given
import lucuma.core.model.CallForProposals

// TODO move to lucuma-core
enum CallForProposalType(val tag: String) derives Enumerated:
  case DemoScience        extends CallForProposalType("DemoScience")
  case DirectorsTime      extends CallForProposalType("DirectorsTime")
  case FastTurnaround     extends CallForProposalType("FastTurnaround")
  case LargeProgram       extends CallForProposalType("LargeProgram")
  case PoorWeather        extends CallForProposalType("PoorWeather")
  case RegularSemester    extends CallForProposalType("RegularSemester")
  case SystemVerification extends CallForProposalType("SystemVerification")

case class CallForProposal(
  id:       CallForProposals.Id,
  semester: Semester,
  title:    NonEmptyString,
  cfpType:  CallForProposalType
) derives Eq,
      Decoder
