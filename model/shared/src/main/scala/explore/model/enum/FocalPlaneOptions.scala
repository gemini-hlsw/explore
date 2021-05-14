// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class FocalPlaneOptions(val label: String) extends Product with Serializable

object FocalPlaneOptions {
  case object SingleSlit extends FocalPlaneOptions("Single Slit")
  case object MultiSlit  extends FocalPlaneOptions("Multiple Slits")
  case object IFU        extends FocalPlaneOptions("IFU")

  implicit val ConfigurationModeEnumerated: Enumerated[FocalPlaneOptions] =
    Enumerated.of(SingleSlit, MultiSlit, IFU)
}
