// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import explore.common.ObsQueries
import explore.common.ObsQueries._
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Asterism
import explore.model.ConstraintGroup
import explore.model.GridLayoutSection
import explore.model.ModelUndoStacks
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.model.TargetSummary
import explore.model.display._
import explore.model.enums.AppTab
import explore.model.layout._
import explore.optics._
import explore.optics.all._
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.all._
import queries.common.ObsQueriesGQL._
import react.common._
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.modules.dropdown.Dropdown

import java.time.Instant
import scala.collection.immutable.SortedMap

final case class ObsTabTiles(
  userId:           Option[User.Id],
  programId:        Program.Id,
  obsId:            Observation.Id,
  backButton:       VdomNode,
  constraintGroups: View[ConstraintsList],
  focusedObs:       Option[Observation.Id],
  focusedTarget:    Option[Target.Id],
  targetMap:        SortedMap[Target.Id, TargetSummary],
  undoStacks:       View[ModelUndoStacks[IO]],
  searching:        View[Set[Target.Id]],
  hiddenColumns:    View[Set[String]],
  defaultLayouts:   LayoutsMap,
  layouts:          View[Pot[LayoutsMap]],
  coreWidth:        Int,
  coreHeight:       Int
)(implicit
  val ctx:          AppContextIO
) extends ReactFnProps[ObsTabTiles](ObsTabTiles.component)

object ObsTabTiles {
  type Props = ObsTabTiles

  private def makeConstraintsSelector(
    constraintGroups: View[ConstraintsList],
    obsView:          Pot[View[ObsEditData]]
  )(implicit ctx:     AppContextIO): VdomNode =
    potRender[View[ObsEditData]] { vod =>
      val cgOpt: Option[ConstraintGroup] =
        constraintGroups.get.find(_._1.contains(vod.get.id)).map(_._2)

      Select(
        clazz = ExploreStyles.ConstraintsTileSelector,
        value = cgOpt.map(cg => ObsIdSet.fromString.reverseGet(cg.obsIds)).orEmpty,
        onChange = (p: Dropdown.DropdownProps) => {
          val newCgOpt =
            ObsIdSet.fromString
              .getOption(p.value.toString)
              .flatMap(ids => constraintGroups.get.get(ids))
          newCgOpt.map { cg =>
            vod
              .zoom(ObsEditData.scienceData.andThen(ScienceData.constraints))
              .set(cg.constraintSet) >>
              ObsQueries
                .updateObservationConstraintSet[IO](List(vod.get.id), cg.constraintSet)
                .runAsyncAndForget
          }.getOrEmpty
        },
        options = constraintGroups.get
          .map(kv =>
            new SelectItem(
              value = ObsIdSet.fromString.reverseGet(kv._1),
              text = kv._2.constraintSet.shortName
            )
          )
          .toList
      )
    }(obsView)

  private def otherObsCount(
    targetObsMap: SortedMap[Target.Id, TargetSummary],
    obsId:        Observation.Id,
    targetId:     Target.Id
  ): Int =
    targetObsMap.get(targetId).fold(0)(summary => (summary.obsIds - obsId).size)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStreamResourceViewOnMountBy { props =>
        implicit val ctx = props.ctx

        ObsEditQuery
          .query(props.obsId)
          .map(
            (ObsEditQuery.Data.asObsEditData.get _)
              .andThen(_.getOrElse(throw new Exception(s"Observation [${props.obsId}] not found")))
          )
          .reRunOnResourceSignals(ObservationEditSubscription.subscribe[IO](props.obsId))
      }
      .render { (props, obsView) =>
        implicit val ctx                     = props.ctx
        val scienceMode: Option[ScienceMode] =
          obsView.toOption.flatMap(_.get.scienceData.mode)

        val posAngle = obsView.toOption.flatMap(_.get.scienceData.posAngle)

        val potAsterism: Pot[View[Option[Asterism]]] =
          obsView.map(v =>
            v.zoom(
              ObsEditData.scienceData
                .andThen(ScienceData.targets)
                .andThen(ObservationData.TargetEnvironment.asterism)
            ).zoom(Asterism.fromTargetsList.asLens)
          )

        val vizTimeView: Pot[View[Option[Instant]]] =
          obsView.map(_.zoom(ObsEditData.visualizationTime))

        val potAsterismMode: Pot[(View[Option[Asterism]], Option[ScienceMode])] =
          potAsterism.map(x => (x, scienceMode))

        val targetCoords: Option[(Target.Id, Coordinates)] =
          potAsterism.toOption
            .flatMap(
              _.get.flatMap(t =>
                t.baseTarget.target match {
                  case Target.Sidereal(_, tracking, _, _) =>
                    (t.baseTarget.id, tracking.baseCoordinates).some
                  case _                                  => none
                }
              )
            )

        val notesTile =
          Tile(
            ObsTabTilesIds.NotesId,
            s"Note for Observer",
            props.backButton.some,
            canMinimize = true
          )(_ =>
            <.div(
              ExploreStyles.NotesWrapper,
              <.div(
                ExploreStyles.ObserverNotes,
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
              )
            )
          )

        val constraintsSelector = makeConstraintsSelector(props.constraintGroups, obsView)

        val skyPlotTile =
          ElevationPlotTile.elevationPlotTile(props.userId, scienceMode, targetCoords)

        def setCurrentTarget(programId: Program.Id, oid: Option[Observation.Id])(
          tid:                          Option[Target.Id],
          via:                          SetRouteVia
        ): Callback =
          ctx.setPageVia(AppTab.Observations, programId, oid.map(ObsIdSet.one(_)), tid, via)

        val targetTile = AsterismEditorTile.asterismEditorTile(
          props.userId,
          props.programId,
          ObsIdSet.one(props.obsId),
          potAsterismMode,
          vizTimeView,
          posAngle,
          obsView.toOption.map(_.get.scienceData.constraints),
          obsView.toOption.flatMap(_.get.scienceData.requirements.spectroscopy.wavelength),
          props.focusedTarget,
          setCurrentTarget(props.programId, props.focusedObs),
          otherObsCount(props.targetMap, props.obsId, _),
          props.undoStacks.zoom(ModelUndoStacks.forSiderealTarget),
          props.searching,
          "Targets",
          none,
          props.hiddenColumns
        )

        // The ExploreStyles.ConstraintsTile css adds a z-index to the constraints tile react-grid wrapper
        // so that the constraints selector dropdown always appears in front of any other tiles. If more
        // than one tile ends up having dropdowns in the tile header, we'll need something more complex such
        // as changing the css classes on the various tiles when the dropdown is clicked to control z-index.
        val constraintsTile =
          ConstraintsTile.constraintsTile(
            props.obsId,
            obsView.map(_.zoom(ObsEditData.scienceData.andThen(ScienceData.constraints))),
            props.undoStacks
              .zoom(ModelUndoStacks.forConstraintGroup[IO])
              .zoom(atMapWithDefault(ObsIdSet.one(props.obsId), UndoStacks.empty)),
            control = constraintsSelector.some,
            clazz = ExploreStyles.ConstraintsTile.some
          )

        val configurationTile =
          ConfigurationTile.configurationTile(
            props.obsId,
            obsView.map(obsEditData =>
              (obsEditData.get.title,
               obsEditData.get.subtitle,
               obsEditData.zoom(ObsEditData.scienceData)
              )
            ),
            props.undoStacks
              .zoom(ModelUndoStacks.forObservationData[IO])
              .zoom(atMapWithDefault(props.obsId, UndoStacks.empty))
          )

        val rglRender: LayoutsMap => VdomNode = (l: LayoutsMap) =>
          TileController(
            props.userId,
            props.coreWidth,
            props.defaultLayouts,
            l,
            List(
              notesTile,
              targetTile,
              skyPlotTile,
              constraintsTile,
              configurationTile
            ),
            GridLayoutSection.ObservationsLayout,
            clazz = ExploreStyles.ObservationTiles.some
          )

        potRenderView[LayoutsMap](rglRender)(props.layouts)
      }

}
