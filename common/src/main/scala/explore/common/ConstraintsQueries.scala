// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import clue.data.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ObsQueries._
import explore.common.ObsQueriesGQL._
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ElevationRange
import explore.model.HourAngleRange
import explore.schemas.ObservationDB.Types._
import explore.undo.UndoableView
import lucuma.core.enum._
import lucuma.core.model.Observation
import monocle.Lens

object ConstraintsQueries {
  case class UndoView(
    obsId:        Observation.Id,
    undoCtx:      UndoCtx[ConstraintSetData]
  )(implicit ctx: AppContextIO) {
    private val undoableView = UndoableView(undoCtx)

    def apply[A](
      modelGet:  ConstraintSetData => A,
      modelMod:  (A => A) => ConstraintSetData => ConstraintSetData,
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      undoableView.apply(
        modelGet,
        modelMod,
        value =>
          UpdateConstraintSetMutation
            .execute(obsId, remoteSet(value)(EditConstraintSetInput()))
            .void
      )

    def apply[A](
      lens:      Lens[ConstraintSetData, A],
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)
  }

  object UpdateConstraintSet {
    def name(n: NonEmptyString): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.name.replace(n.assign)

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
