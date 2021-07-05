package explore.model.enum

import lucuma.core.util.Enumerated

sealed trait FocalPlane extends Product with Serializable {
  def label: String
}

object FocalPlane {
  case object SingleSlit   extends FocalPlane {
    val label = "Single Slit"
  }
  case object MultipleSlit extends FocalPlane {
    val label = "Multiple Slits"
  }
  case object IFU          extends FocalPlane {
    val label = "IFU"
  }

  /** @group Typeclass Instances */
  implicit val FocalPlaneEnumerated: Enumerated[FocalPlane] =
    Enumerated.of(SingleSlit, MultipleSlit, IFU)
}
