// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.GmosSequenceTable
import explore.config.sequence.GmosSequenceTableBuilder
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.table.ColumnSize.*

case class GmosNorthSequenceTable(
  visits:     List[Visit.GmosNorth],
  config:     ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
  snPerClass: Map[SequenceType, (SingleSN, TotalSN)]
) extends ReactFnProps(GmosNorthSequenceTable.component)
    with GmosSequenceTable[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]

object GmosNorthSequenceTable
    extends GmosSequenceTableBuilder[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]
