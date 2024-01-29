// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.odb.json.sequence.given
import lucuma.odb.json.timeaccounting.given

case class Execution(digest: Option[ExecutionDigest], timeCharge: CategorizedTime)
    derives Decoder,
      Eq
