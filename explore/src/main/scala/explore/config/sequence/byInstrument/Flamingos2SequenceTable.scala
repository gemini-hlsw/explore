// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit

case class Flamingos2SequenceTable(
  visits:     List[Visit.Flamingos2],
  config:     ExecutionConfig.Flamingos2,
  snPerClass: Map[SequenceType, (SingleSN, TotalSN)]
) extends ReactFnProps(Flamingos2SequenceTable.component)
    with SequenceTable[Flamingos2StaticConfig, Flamingos2DynamicConfig]

object Flamingos2SequenceTable
    extends SequenceTableBuilder[Flamingos2StaticConfig, Flamingos2DynamicConfig](
      Instrument.Flamingos2
    )
