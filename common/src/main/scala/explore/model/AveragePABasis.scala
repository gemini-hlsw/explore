// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Angle
import lucuma.core.util.TimeSpan
import org.typelevel.cats.time.given

import java.time.Instant

case class AveragePABasis(
  when:      Instant,
  duration:  TimeSpan,
  averagePA: Angle
) derives Eq
