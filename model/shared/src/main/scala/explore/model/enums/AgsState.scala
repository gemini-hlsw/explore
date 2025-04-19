// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq

enum AgsState:
  case Idle, LoadingCandidates, Calculating, Saving, Error

  def fold[A](idle: => A, loading: => A, calculating: => A, saving: => A, error: => A): A =
    this match
      case Idle              => idle
      case LoadingCandidates => loading
      case Calculating       => calculating
      case Saving            => saving
      case Error             => error

  def isCalculating: Boolean = fold(false, false, true, false, false)

  def isIdle: Boolean = fold(true, false, false, false, false)

  def canRecalculate: Boolean = this match
    case Idle | Error => true
    case _            => false

object AgsState:
  /** @group Typeclass Instances */
  given Eq[AgsState] = Eq.fromUniversalEquals
