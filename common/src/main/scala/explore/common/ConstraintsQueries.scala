// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import cats.syntax.all.given
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries.ProgramSummaries
import explore.data.KeyedIndexedList
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ObservationList
import explore.undo.UndoContext
import lucuma.core.enums.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Iso
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL.*

object ConstraintsQueries:
  case class UndoView(
    programId: Program.Id,
    obsIds:    ObsIdSet,
    undoCtx:   UndoContext[ObservationList]
  )(using FetchClient[IO, ?, ObservationDB], Logger[IO]):
    def apply[A](
      modelGet:  ConstraintSet => A,
      modelMod:  (A => A) => ConstraintSet => ConstraintSet,
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      val traversal =
        Iso
          .id[ObservationList]
          .filterIndex((id: Observation.Id) => obsIds.idSet.contains_(id))
          .andThen(KeyedIndexedList.value)
          .andThen(ObsSummary.constraints)

      undoCtx
        // This deserves an explanation:
        // The traversal provides a View over observations that have the same
        // ConstraintSet. Therefore, we can see this as a view over a single
        // ConstraintSet. We get the value from any of them (eg: head), and
        // we set it in all of them.
        .zoom(traversal.getAll.andThen(_.head), traversal.modify)
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateObservationMutation[IO]
            .execute(
              UpdateObservationsInput(
                programId = programId,
                WHERE =
                  obsIds.toList.toWhereObservation.assign, // TODO should toWhereObservation accept NEL???
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
