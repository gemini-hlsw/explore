// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedMap

case class Allocation(
  category:    TimeAccountingCategory,
  scienceBand: ScienceBand,
  duration:    TimeSpan
) derives Eq

object Allocation:
  val category: Lens[Allocation, TimeAccountingCategory] = Focus[Allocation](_.category)
  val scienceBand: Lens[Allocation, ScienceBand]         = Focus[Allocation](_.scienceBand)
  val duration: Lens[Allocation, TimeSpan]               = Focus[Allocation](_.duration)

  given Decoder[Allocation] = deriveDecoder

object BandAllocations extends NewType[SortedMap[ScienceBand, TimeSpan]]
type BandAllocations = BandAllocations.Type

object CategoryAllocationList extends NewType[SortedMap[TimeAccountingCategory, BandAllocations]]:
  def fromAllocations(allocations: List[Allocation]): CategoryAllocationList =
    CategoryAllocationList:
      SortedMap.from:
        allocations
          .groupBy(_.category)
          .view
          .mapValues: as =>
            BandAllocations:
              SortedMap.from:
                as.map(a => a.scienceBand -> a.duration)

  given Decoder[CategoryAllocationList] = Decoder.decodeList[Allocation].map(fromAllocations)

type CategoryAllocationList = CategoryAllocationList.Type
