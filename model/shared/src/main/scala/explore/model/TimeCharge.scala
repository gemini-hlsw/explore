// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

final case class TimeCharge(program: TimeSpan) derives Decoder, Eq
