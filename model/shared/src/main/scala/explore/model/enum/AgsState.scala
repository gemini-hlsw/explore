// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum AgsState:
  case Idle, LoadingCandidates, Calculating, Error

object AgsState:
  /** @group Typeclass Instances */
  implicit val AgsStateEnumerated: Enumerated[AgsState] =
    Enumerated.of(Idle, LoadingCandidates, Calculating, Error)
