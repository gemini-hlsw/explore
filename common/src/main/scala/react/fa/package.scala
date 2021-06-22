// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react

import react.common.EnumValue

package fa {
  sealed trait Family extends Product with Serializable {
    val prefix: String
  }
  object Family {
    implicit val enum: EnumValue[Family] = EnumValue.toLowerCaseString
    case object Solid   extends Family {
      val prefix = "fas"
    }
    case object Regular extends Family {
      val prefix = "far"
    }
    case object Light   extends Family {
      val prefix = "fal"
    }
    case object Duotone extends Family {
      val prefix = "fad"
    }

    def fromString(f: String): Family = f match {
      case "far" => Regular
      case "fal" => Light
      case "fad" => Duotone
      case _     => Solid
    }
  }

  sealed trait Flip extends Product with Serializable
  object Flip {
    implicit val enum: EnumValue[Flip] = EnumValue.toLowerCaseString
    case object Horizontal extends Flip
    case object Vertical   extends Flip
    case object Both       extends Flip
  }

  sealed trait IconSize extends Product with Serializable
  object IconSize {
    implicit val enum: EnumValue[IconSize] = EnumValue.instance {
      case XS  => "xs"
      case LG  => "lg"
      case SM  => "sm"
      case X1  => "1x"
      case X2  => "2x"
      case X3  => "3x"
      case X4  => "4x"
      case X5  => "5x"
      case X6  => "6x"
      case X7  => "7x"
      case X8  => "8x"
      case X9  => "9x"
      case X10 => "10x";
    }
    case object XS extends IconSize
    case object LG  extends IconSize
    case object SM  extends IconSize
    case object X1  extends IconSize
    case object X2  extends IconSize
    case object X3  extends IconSize
    case object X4  extends IconSize
    case object X5  extends IconSize
    case object X6  extends IconSize
    case object X7  extends IconSize
    case object X8  extends IconSize
    case object X9  extends IconSize
    case object X10 extends IconSize;
  }

  sealed trait Pull extends Product with Serializable
  object Pull {
    implicit val enum: EnumValue[Pull] = EnumValue.toLowerCaseString
    case object Left  extends Pull
    case object Right extends Pull
  }

  sealed trait Rotation extends Product with Serializable
  object Rotation {
    case object Rotate90  extends Rotation
    case object Rotate180 extends Rotation
    case object Rotate270 extends Rotation
  }

}
