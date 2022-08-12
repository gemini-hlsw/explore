// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated

enum ItcSeriesDataType(val tag: String):
  case SignalData     extends ItcSeriesDataType("signal_data")
  case BackgroundData extends ItcSeriesDataType("background_data")
  case SingleS2NData  extends ItcSeriesDataType("single_s2_ndata")
  case FinalS2NData   extends ItcSeriesDataType("final_s2_ndata")
  case PixSigData     extends ItcSeriesDataType("pix_sig_data")
  case PixBackData    extends ItcSeriesDataType("pick_back_data")

object ItcSeriesDataType:
  given Enumerated[ItcSeriesDataType] =
    Enumerated
      .from(
        ItcSeriesDataType.SignalData,
        ItcSeriesDataType.BackgroundData,
        ItcSeriesDataType.SingleS2NData,
        ItcSeriesDataType.FinalS2NData,
        ItcSeriesDataType.PixSigData,
        ItcSeriesDataType.PixBackData
      )
      .withTag(_.tag)
