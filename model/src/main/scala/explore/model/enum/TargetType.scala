package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class TargetType extends Product with Serializable

object TargetType {

  case object Sidereal extends TargetType

  /** @group Typeclass Instances */
  implicit val TargetTypeEnumerated: Enumerated[TargetType] =
    Enumerated.of(Sidereal)

}
