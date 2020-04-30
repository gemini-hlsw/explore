// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.util.UUID
import io.circe.generic.JsonCodec
import japgolly.scalajs.react.Reusability

@JsonCodec
case class PollOption(id: UUID, text: String)

object PollOption {
  implicit val reuse: Reusability[PollOption] = Reusability.derive[PollOption]
}

@JsonCodec
case class Poll(id: UUID, question: String, options: List[PollOption])

object Poll       {
  implicit val reuse: Reusability[Poll] = Reusability.derive[Poll]
}
