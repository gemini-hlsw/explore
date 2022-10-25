// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import queries.common.TargetQueriesGQL
import queries.schemas.odb.ODBConversions.*

object TargetSummaryActions {

  def deleteTargets(targetId: List[Target.Id], programId: Program.Id)(using
    c:                        TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    TargetQueriesGQL.DeleteTargetsMutation
      .execute[IO](
        DeleteTargetsInput(WHERE =
          targetId.toWhereTargets
            .copy(programId = WhereOrderProgramId(programId.assign).assign)
            .assign
        )
      )
      .void

}
