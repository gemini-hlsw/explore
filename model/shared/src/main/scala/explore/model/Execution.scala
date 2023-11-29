// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import io.circe.Decoder
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.odb.json.sequence.given

case class Execution(digest: Option[ExecutionDigest])

object Execution:
  given Decoder[Execution] =
    Decoder.instance(_.downField("digest").as[Option[ExecutionDigest]].map(Execution(_)))
