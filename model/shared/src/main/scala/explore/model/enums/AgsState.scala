// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq

enum AgsState:
  case Idle, LoadingCandidates, Calculating, Error

  def canRecalculate: Boolean = this match
    case Idle | Error => true
    case _            => false

object AgsState:
  /** @group Typeclass Instances */
  given Eq[AgsState] = Eq.fromUniversalEquals
