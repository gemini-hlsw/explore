// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.util.NewType
import monocle.Focus
import monocle.Lens
import explore.model.enums.SortDirection

object ColumnId extends NewType[String]
type ColumnId = ColumnId.Type

case class TableColumnPref(
  columnId: ColumnId,
  visible:  Boolean,
  sorting:  Option[SortDirection]
) derives Eq,
      Decoder

object TableColumnPref:
  def apply(columnId: String): TableColumnPref = TableColumnPref(ColumnId(columnId), false, None)

  val ColumnIdLens: Lens[TableColumnPref, ColumnId] = Focus[TableColumnPref](_.columnId)
