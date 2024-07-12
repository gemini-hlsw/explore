// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedMap

case class Allocation(partner: Partner, scienceBand: ScienceBand, duration: TimeSpan) derives Eq

object Allocation:
  val partner: Lens[Allocation, Partner]         = Focus[Allocation](_.partner)
  val scienceBand: Lens[Allocation, ScienceBand] = Focus[Allocation](_.scienceBand)
  val duration: Lens[Allocation, TimeSpan]       = Focus[Allocation](_.duration)

  given Decoder[Allocation] = deriveDecoder

object PartnerAllocations extends NewType[SortedMap[ScienceBand, TimeSpan]]
type PartnerAllocations = PartnerAllocations.Type

object PartnerAllocationList extends NewType[SortedMap[Partner, PartnerAllocations]]:
  def fromAllocations(allocations: List[Allocation]): PartnerAllocationList =
    PartnerAllocationList:
      SortedMap.from:
        allocations
          .groupBy(_.partner)
          .view
          .mapValues: as =>
            PartnerAllocations:
              SortedMap.from:
                as.map(a => a.scienceBand -> a.duration)

  given Decoder[PartnerAllocationList] = Decoder.decodeList[Allocation].map(fromAllocations)

type PartnerAllocationList = PartnerAllocationList.Type
