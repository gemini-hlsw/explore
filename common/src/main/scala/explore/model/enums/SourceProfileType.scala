// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.model.SourceProfile
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class SourceProfileType(
  val name:    String,
  val convert: SourceProfile => SourceProfile
) extends Product
    with Serializable

object SourceProfileType {
  import SourceProfile.*

  case object PointType    extends SourceProfileType("Point", _.toPoint)
  case object UniformType  extends SourceProfileType("Uniform", _.toUniform)
  case object GaussianType extends SourceProfileType("Gaussian", _.toGaussian)

  def fromSourceProfile(sourceProfile: SourceProfile): SourceProfileType = sourceProfile match {
    case Point(_)       => SourceProfileType.PointType
    case Uniform(_)     => SourceProfileType.UniformType
    case Gaussian(_, _) => SourceProfileType.GaussianType
  }

  implicit val enumSourceProfileType: Enumerated[SourceProfileType] =
    Enumerated.from(PointType, UniformType, GaussianType).withTag(_.name)

  implicit val displaySourceProfileType: Display[SourceProfileType] = Display.byShortName(_.name)
}
