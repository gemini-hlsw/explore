// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

sealed abstract class TargetType extends Product with Serializable

object TargetType {

  case object Sidereal extends TargetType

  /** @group Typeclass Instances */
  implicit val TargetTypeEnumerated: Enumerated[TargetType] =
    Enumerated.of(Sidereal)

}
