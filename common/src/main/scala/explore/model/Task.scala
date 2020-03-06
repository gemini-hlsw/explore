// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import japgolly.scalajs.react.Reusability

case class Task(id: String, title: String, completed: Boolean)
object Task {
  implicit val jsonDecoder: Decoder[Task] = deriveDecoder[Task]
  implicit val jsonEncoder: Encoder[Task] = deriveEncoder[Task]

  implicit val reuse: Reusability[Task] = Reusability.derive
}
