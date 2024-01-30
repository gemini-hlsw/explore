// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.ProgramTime
import io.circe.Decoder
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.odb.json.sequence.given

case class Execution(digest: Option[ExecutionDigest], programTimeCharge: ProgramTime) derives Eq

object Execution {
  given Decoder[Execution] = Decoder.instance(c =>
    for {
      d  <- c.get[Option[ExecutionDigest]]("digest")
      pt <- c.get[ProgramTime]("timeCharge")
    } yield Execution(d, pt)
  )
}
