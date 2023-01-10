// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import lucuma.core.util.NewType

object PlotDetails extends NewType[Boolean]:
  val Shown: PlotDetails  = PlotDetails(true)
  val Hidden: PlotDetails = PlotDetails(false)

type PlotDetails = PlotDetails.Type
