// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.undo.UndoContext
import lucuma.core.enums.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ODBConversions.*

object ConstraintsQueries:
  case class UndoView(
    programId: Program.Id,
    obsIds:    List[Observation.Id],
    undoCtx:   UndoContext[ConstraintSet]
  )(using TransactionalClient[IO, ObservationDB], Logger[IO]):
    def apply[A](
      modelGet:  ConstraintSet => A,
      modelMod:  (A => A) => ConstraintSet => ConstraintSet,
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateObservationMutation
            .execute(
              UpdateObservationsInput(
                programId = programId,
                WHERE = obsIds.toWhereObservation.assign,
                SET = ObservationPropertiesInput(
                  constraintSet = remoteSet(value)(ConstraintSetInput()).assign
                )
              )
            )
            .void
            .runAsync
        )

    def apply[A](
      lens:      Lens[ConstraintSet, A],
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)

  object UpdateConstraintSet:
    def imageQuality(iq: ImageQuality): Endo[ConstraintSetInput] =
      ConstraintSetInput.imageQuality.replace(iq.assign)

    def cloudExtinction(ce: CloudExtinction): Endo[ConstraintSetInput] =
      ConstraintSetInput.cloudExtinction.replace(ce.assign)

    def skyBackground(sb: SkyBackground): Endo[ConstraintSetInput] =
      ConstraintSetInput.skyBackground.replace(sb.assign)

    def waterVapor(wv: WaterVapor): Endo[ConstraintSetInput] =
      ConstraintSetInput.waterVapor.replace(wv.assign)

    def elevationRange(er: ElevationRange): Endo[ConstraintSetInput] = {
      val createER: ElevationRangeInput = er match {
        case ElevationRange.AirMass(min, max)   =>
          ElevationRangeInput(
            // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
            airMass = AirMassRangeInput(min = PosBigDecimal.unsafeFrom(min.value).assign,
                                        max = PosBigDecimal.unsafeFrom(max.value).assign
            ).assign
          )
        case ElevationRange.HourAngle(min, max) =>
          ElevationRangeInput(hourAngle =
            HourAngleRangeInput(minHours = min.value.assign, maxHours = max.value.assign).assign
          )
      }
      ElevationRangeInput()
      ConstraintSetInput.elevationRange.replace(createER.assign)
    }
