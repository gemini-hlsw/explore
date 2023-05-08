// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.syntax.all.*
import explore.model.TargetList
import explore.model.itc.ItcTarget
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.optics.all.*
import lucuma.core.model.*
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import queries.schemas.odb.ObsQueries

trait syntax:

  extension (row: InstrumentRow)
    def toItcClientMode: Option[InstrumentMode] = row match {
      case g: GmosNorthSpectroscopyRow =>
        InstrumentMode.GmosNorthSpectroscopy(g.grating, g.filter, GmosFpu.North(g.fpu.asRight)).some
      case g: GmosSouthSpectroscopyRow =>
        InstrumentMode.GmosSouthSpectroscopy(g.grating, g.filter, GmosFpu.South(g.fpu.asRight)).some
      case _                           => None
    }

  extension (s: ObsQueries.ScienceData)
    // From the list of targets selects the ones relevant for ITC
    def itcTargets(allTargets: TargetList): List[ItcTarget] = s.targets.asterism
      .map(_.id)
      .map(targetId =>
        allTargets
          .get(targetId)
          .flatMap(target =>
            targetRV
              .getOption(target)
              .map(r => ItcTarget(target.name, r, Target.sourceProfile.get(target)))
          )
      )
      .flatten
      .hashDistinct

object syntax extends syntax
