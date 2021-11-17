// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import clue.data.syntax._
import crystal.react.implicits._
import explore.common.ObsQueriesGQL._
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ConstraintSet
import explore.model.ElevationRange
import explore.model.HourAngleRange
import explore.undo.UndoContext
import lucuma.core.enum._
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens

object ConstraintsQueries {
  case class UndoView(
    obsIds:       List[Observation.Id],
    undoCtx:      UndoContext[ConstraintSet]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      modelGet:  ConstraintSet => A,
      modelMod:  (A => A) => ConstraintSet => ConstraintSet,
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateConstraintSetMutation
            .execute(obsIds, remoteSet(value)(EditConstraintSetInput()))
            .void
            .runAsync
        )

    def apply[A](
      lens:      Lens[ConstraintSet, A],
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)
  }

  object UpdateConstraintSet {
    def imageQuality(iq: ImageQuality): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.imageQuality.replace(iq.assign)

    def cloudExtinction(ce: CloudExtinction): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.cloudExtinction.replace(ce.assign)

    def skyBackground(sb: SkyBackground): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.skyBackground.replace(sb.assign)

    def waterVapor(wv: WaterVapor): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.waterVapor.replace(wv.assign)

    def elevationRange(er: ElevationRange): Endo[EditConstraintSetInput] = {
      val createER: CreateElevationRangeInput = er match {
        case AirMassRange(min, max)   =>
          CreateElevationRangeInput(airmassRange =
            CreateAirmassRangeInput(min = min.value, max = max.value).assign
          )
        case HourAngleRange(min, max) =>
          CreateElevationRangeInput(hourAngleRange =
            CreateHourAngleRangeInput(minHours = min.value, maxHours = max.value).assign
          )
      }
      CreateElevationRangeInput()
      EditConstraintSetInput.elevationRange.replace(createER.assign)
    }
  }
}
