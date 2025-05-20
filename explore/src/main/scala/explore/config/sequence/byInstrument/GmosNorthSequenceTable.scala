// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit

case class GmosNorthSequenceTable(
  visits:     List[Visit.GmosNorth],
  config:     ExecutionConfig.GmosNorth,
  snPerClass: Map[SequenceType, (SingleSN, TotalSN)]
) extends ReactFnProps(GmosNorthSequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]

object GmosNorthSequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
      _.forGmos
    )
