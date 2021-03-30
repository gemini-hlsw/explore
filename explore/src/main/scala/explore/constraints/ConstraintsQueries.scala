// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Endo
import cats.effect.IO
import clue.GraphQLOperation
import clue.data.syntax._
import clue.macros.GraphQL
import eu.timepit.refined.types.string.NonEmptyString
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ConstraintSetModel
import explore.model.ElevationRange
import explore.model.HourAngleRange
import explore.model.reusability._
import explore.undo.UndoableView
import explore.undo.Undoer
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import lucuma.ui.reusability._
import monocle.Lens

object ConstraintsQueries {
  @GraphQL
  object ConstraintSetQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($id: ConstraintSetId!) {
        constraintSet(constraintSetId: $id) {
          id
          name
          cloudExtinction
          imageQuality
          skyBackground
          waterVapor
          elevationRange {
            type: __typename
            ... on AirMassRange {
              min
              max
            }
            ... on HourAngleRange {
              minHours
              maxHours
            }
          }
        }
      }
      """

    object Data {
      type ConstraintSet = ConstraintSetModel
    }
  }

  @GraphQL
  object ConstraintSetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($id: ConstraintSetId!) {
        constraintSetEdit(constraintSetId: $id) {
          id
        }
      }
    """
  }
  @GraphQL
  object Mutation                      extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($input: EditConstraintSetInput!){
        updateConstraintSet(input: $input) {
          id
        }
      }
    """
  }

  case class UndoView(
    id:           ConstraintSet.Id,
    view:         View[ConstraintSetModel],
    setter:       Undoer.Setter[IO, ConstraintSetModel]
  )(implicit ctx: AppContextIO) {
    private val undoableView = UndoableView(view, setter)

    def apply[A](
      modelGet:  ConstraintSetModel => A,
      modelMod:  (A => A) => ConstraintSetModel => ConstraintSetModel,
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      undoableView.apply(
        modelGet,
        modelMod,
        value => Mutation.execute(remoteSet(value)(EditConstraintSetInput(id))).void
      )

    def apply[A](
      lens:      Lens[ConstraintSetModel, A],
      remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)

  }

  object UpdateConstraintSet {
    def name(n: NonEmptyString): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.name.set(n.assign)

    def imageQuality(iq: ImageQuality): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.imageQuality.set(iq.assign)

    def cloudExtinction(ce: CloudExtinction): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.cloudExtinction.set(ce.assign)

    def skyBackground(sb: SkyBackground): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.skyBackground.set(sb.assign)

    def waterVapor(wv: WaterVapor): Endo[EditConstraintSetInput] =
      EditConstraintSetInput.waterVapor.set(wv.assign)

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
      EditConstraintSetInput.elevationRange.set(createER.assign)
    }
  }
}
