// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.GmosSequenceTable
import explore.config.sequence.GmosSequenceTableBuilder
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.table.ColumnSize.*

case class GmosSouthSequenceTable(
  visits:     List[Visit.GmosSouth],
  config:     ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
  snPerClass: Map[SequenceType, SignalToNoise]
) extends ReactFnProps(GmosSouthSequenceTable.component)
    with GmosSequenceTable[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]

object GmosSouthSequenceTable
    extends GmosSequenceTableBuilder[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]
