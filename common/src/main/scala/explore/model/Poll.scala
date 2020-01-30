package explore.model

import java.util.UUID
import io.circe.generic.JsonCodec

@JsonCodec
case class PollOption(id: UUID, text: String)

@JsonCodec
case class Poll(id: UUID, question: String, options: List[PollOption])
