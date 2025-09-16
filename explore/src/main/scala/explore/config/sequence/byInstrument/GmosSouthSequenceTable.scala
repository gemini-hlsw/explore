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
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit

case class GmosSouthSequenceTable(
  visits:     List[Visit.GmosSouth],
  config:     ExecutionConfig.GmosSouth,
  snPerClass: Map[SequenceType, (SingleSN, TotalSN)]
) extends ReactFnProps(GmosSouthSequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]

object GmosSouthSequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
