// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.syntax.all.*
import explore.model.Observation
import explore.model.ProgramSummaries

private def obsListGetter(
  obsList: List[Observation.Id]
): ProgramSummaries => Option[List[Observation]] =
  programSummaries => obsList.map(programSummaries.observations.get(_)).sequence

private def obsListSetter(obsList: List[Observation.Id])(
  otwol: Option[List[Observation]]
): ProgramSummaries => ProgramSummaries =
  programSummaries =>
    otwol.fold {
      // the Option[List]] is empty, so we're deleting.
      obsList.foldLeft(programSummaries) { case (grps, obsId) => grps.removeObs(obsId) }
    } {
      // we insert the ones we received back into the programSummaries
      _.foldLeft(programSummaries)((grps, obsSumm) => grps.insertObs(obsSumm))
    }
