// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum ItcSeriesDataType:
  case SignalData, BackgroundData, SingleS2NData, FinalS2NData, PixSigData, PixBackData

object ItcSeriesDataType:
  given Enumerated[ItcSeriesDataType] =
    Enumerated.of(
      ItcSeriesDataType.SignalData,
      ItcSeriesDataType.BackgroundData,
      ItcSeriesDataType.SingleS2NData,
      ItcSeriesDataType.FinalS2NData,
      ItcSeriesDataType.PixSigData,
      ItcSeriesDataType.PixBackData
    )
