// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.ObsIdSet
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.ToastCtx
import lucuma.core.enums.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL.*

object ConstraintsQueries:
  case class UndoView(
    obsIds:  ObsIdSet,
    undoCtx: UndoSetter[ConstraintSet]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]):
    def apply[A](
      modelGet:  ConstraintSet => A,
      modelMod:  (A => A) => ConstraintSet => ConstraintSet,
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateObservationMutation[IO]
            .execute(
              UpdateObservationsInput(
                WHERE = obsIds.toList.toWhereObservation.assign,
                SET = ObservationPropertiesInput(
                  constraintSet = remoteSet(value)(ConstraintSetInput()).assign
                )
              )
            )
            .toastErrors
            .void
            .runAsync
        )

    def apply[A](
      lens:      Lens[ConstraintSet, A],
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)

  object UpdateConstraintSet:
    def imageQuality(iq: ImageQuality.Preset): Endo[ConstraintSetInput] =
      ConstraintSetInput.imageQuality.replace(iq.assign)

    def cloudExtinction(ce: CloudExtinction.Preset): Endo[ConstraintSetInput] =
      ConstraintSetInput.cloudExtinction.replace(ce.assign)

    def skyBackground(sb: SkyBackground): Endo[ConstraintSetInput] =
      ConstraintSetInput.skyBackground.replace(sb.assign)

    def waterVapor(wv: WaterVapor): Endo[ConstraintSetInput] =
      ConstraintSetInput.waterVapor.replace(wv.assign)

    def elevationRange(er: ElevationRange): Endo[ConstraintSetInput] = {
      val createER: ElevationRangeInput = er match {
        case ElevationRange.ByAirMass(min, max)   =>
          ElevationRangeInput(
            // These are actually safe, because min and max in the model are refined [1.0 - 3.0]
            airMass = AirMassRangeInput(
              min = PosBigDecimal.unsafeFrom(min.toBigDecimal).assign,
              max = PosBigDecimal.unsafeFrom(max.toBigDecimal).assign
            ).assign
          )
        case ElevationRange.ByHourAngle(min, max) =>
          ElevationRangeInput(hourAngle =
            HourAngleRangeInput(
              minHours = min.toBigDecimal.assign,
              maxHours = max.toBigDecimal.assign
            ).assign
          )
      }
      ElevationRangeInput()
      ConstraintSetInput.elevationRange.replace(createER.assign)
    }
