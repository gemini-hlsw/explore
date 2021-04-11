// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.types.string

object Help {
  type Id = string.NonEmptyString

  implicit val eqId: Eq[Id] = Eq.by(_.value)
}
