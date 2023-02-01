// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.itc.ItcChartExposureTime
import monocle.Focus
import monocle.Lens

case class BasicConfigAndItc(
  configuration: BasicConfiguration,
  exposureTime:  Option[ItcChartExposureTime]
) derives Eq

object BasicConfigAndItc:
  val configuration: Lens[BasicConfigAndItc, BasicConfiguration]          =
    Focus[BasicConfigAndItc](_.configuration)
  val exposureTime: Lens[BasicConfigAndItc, Option[ItcChartExposureTime]] =
    Focus[BasicConfigAndItc](_.exposureTime)
