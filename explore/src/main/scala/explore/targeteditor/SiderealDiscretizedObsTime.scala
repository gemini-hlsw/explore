// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.PosAngleConstraint
import org.typelevel.cats.time.instances.all.*

import java.time.Duration
import java.time.Instant

// Wrapper obsTime and posAngleConstraint, with Reusability to reflect the fact that
// we want to re render only when the obsTime changes at least a month, unless we are using `AverageParallactic` constraint.
// We keep the GS candidates data PM corrected for the obs time.
// If it changes over a month we'll request the data again and recalculate.
// This way we avoid recalculating PM for example if only pos angle or conditions change.
// However, if `AverageParallactic` is used, we need to recompute GS candidates for each obsTime.
case class SiderealDiscretizedObsTime(
  obsTime:            Instant,
  posAngleConstraint: Option[PosAngleConstraint]
)

object SiderealDiscretizedObsTime:
  given Reusability[SiderealDiscretizedObsTime] = Reusability[SiderealDiscretizedObsTime]: (a, b) =>
    (a.posAngleConstraint, b.posAngleConstraint) match
      case (Some(PosAngleConstraint.AverageParallactic), _) => a.obsTime === b.obsTime
      case (_, Some(PosAngleConstraint.AverageParallactic)) => a.obsTime === b.obsTime
      case _                                                => Duration.between(a.obsTime, b.obsTime).toDays().abs < 30L
