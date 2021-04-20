// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import java.time.Duration
import java.time.temporal.ChronoUnit

// TODO Find a way to copy this from common-graphql when scalafix doesn't kick in.
trait Durationµs {
  val microseconds: Long

  lazy val duration: Duration = Duration.of(microseconds, ChronoUnit.MICROS)
}
