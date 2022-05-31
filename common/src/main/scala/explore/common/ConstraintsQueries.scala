// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.implicits._
import explore.undo.UndoContext
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens
import queries.common.ObsQueriesGQL._

object ConstraintsQueries {
  case class UndoView(
    obsIds:       List[Observation.Id],
    undoCtx:      UndoContext[ConstraintSet]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      modelGet:  ConstraintSet => A,
      modelMod:  (A => A) => ConstraintSet => ConstraintSet,
      remoteSet: A => ConstraintSetInput => ConstraintSetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          EditObservationMutation
            .execute(
              EditObservationInput(
                select = ObservationSelectInput(observationIds = obsIds.assign),
                patch = ObservationPropertiesInput(
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
  }

  object UpdateConstraintSet {
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
            // TODO: Change AirMassRange in lucuma-core to use refined types
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
  }
}
