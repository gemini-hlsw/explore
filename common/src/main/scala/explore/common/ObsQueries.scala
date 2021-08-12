// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import explore.model.reusability._
import explore.optics._
import explore.schemas.ObservationDB
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import monocle.Focus
import monocle.Getter
import monocle.Lens
import monocle.macros.GenIso

import ObsQueriesGQL._

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithPointingAndConstraints]

  type WavelengthInput = ObservationDB.Types.WavelengthModelInput
  val WavelengthInput = ObservationDB.Types.WavelengthModelInput
  type ObservationData = ObsEditQuery.Data.Observation
  val ObservationData = ObsEditQuery.Data.Observation
  type ScienceRequirementsData = ObservationData.ScienceRequirements
  val ScienceRequirementsData = ObservationData.ScienceRequirements
  type SpectroscopyRequirementsData = ObservationData.ScienceRequirements.SpectroscopyRequirements
  val SpectroscopyRequirementsData = ObservationData.ScienceRequirements.SpectroscopyRequirements
  type ScienceConfigurationData = ObservationData.ScienceConfiguration
  val ScienceConfigurationData = ObservationData.ScienceConfiguration

  case class ScienceData(
    requirements:  ScienceRequirementsData,
    configuration: Option[ScienceConfigurationData]
  )
  object ScienceData {
    val requirements: Lens[ScienceData, ScienceRequirementsData]           =
      Focus[ScienceData](_.requirements)
    val configuration: Lens[ScienceData, Option[ScienceConfigurationData]] =
      Focus[ScienceData](_.configuration)
    implicit val reusabilityScienceData: Reusability[ScienceData]          = Reusability.derive
  }

  val scienceDataForObs: Lens[ObservationData, ScienceData] =
    disjointZip(ObservationData.scienceRequirements, ObservationData.scienceConfiguration)
      .andThen(GenIso.fields[ScienceData].reverse)

  private def convertPointing(
    pointing: ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget
  ): Pointing =
    pointing match {
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Target(id, name)   =>
        Pointing.PointingTarget(id, name)
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Asterism(id, name) =>
        Pointing.PointingAsterism(id, name, Nil)
    }

  private val programObservationsQueryoObservationListGetter
    : Getter[ProgramObservationsQuery.Data, ObservationList] = data =>
    KeyedIndexedList.fromList(
      data.observations.nodes.map(node =>
        ObsSummaryWithPointingAndConstraints(node.id,
                                             node.observationTarget.map(convertPointing),
                                             node.constraintSet,
                                             node.status,
                                             node.activeStatus,
                                             node.plannedTime.execution
        )
      ),
      ObsSummaryWithPointingAndConstraints.id.get
    )

  implicit class ProgramObservationsQueryDataOps(val self: ProgramObservationsQuery.Data.type)
      extends AnyVal {
    def asObservationList = programObservationsQueryoObservationListGetter
  }

  val ObsLiveQuery =
    ScalaFnComponent[View[ObservationList] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, ProgramObservationsQuery.Data, ObservationList](
          ProgramObservationsQuery.query().reuseAlways,
          (ProgramObservationsQuery.Data.asObservationList.get _).reuseAlways,
          List(
            ProgramObservationsEditSubscription.subscribe[IO]()
          ).reuseAlways
        )(potRender(render))
      }
    )

}
