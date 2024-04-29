// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.Semester
import lucuma.schemas.decoders.given

case class CallForProposal(semester: Semester) derives Eq, Decoder
