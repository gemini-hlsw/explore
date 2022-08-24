// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated

enum ItcSeriesType(val tag: String):
  case SignalData     extends ItcSeriesType("signal_data")
  case BackgroundData extends ItcSeriesType("background_data")
  case SingleS2NData  extends ItcSeriesType("single_s2_ndata")
  case FinalS2NData   extends ItcSeriesType("final_s2_ndata")
  case PixSigData     extends ItcSeriesType("pix_sig_data")
  case PixBackData    extends ItcSeriesType("pick_back_data")

object ItcSeriesType:
  given Enumerated[ItcSeriesType] =
    Enumerated
      .from(
        ItcSeriesType.SignalData,
        ItcSeriesType.BackgroundData,
        ItcSeriesType.SingleS2NData,
        ItcSeriesType.FinalS2NData,
        ItcSeriesType.PixSigData,
        ItcSeriesType.PixBackData
      )
      .withTag(_.tag)

enum ItcChartType(val tag: String):
  case SignalChart extends ItcChartType("signal_chart")
  case S2NChart    extends ItcChartType("s2n_chart")

object ItcChartType:
  given Enumerated[ItcChartType] =
    Enumerated
      .from(
        ItcChartType.SignalChart,
        ItcChartType.S2NChart
      )
      .withTag(_.tag)
