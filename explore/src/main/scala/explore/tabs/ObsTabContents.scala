// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.*
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.modes.SpectroscopyModesMatrix
import explore.observationtree.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import monocle.Iso

object DeckShown extends NewType[Boolean]:
  inline def Shown: DeckShown  = DeckShown(true)
  inline def Hidden: DeckShown = DeckShown(false)

  extension (s: DeckShown)
    def flip: DeckShown =
      if (s.value) DeckShown.Hidden else DeckShown.Shown

type DeckShown = DeckShown.Type

case class ObsTabContents(
  vault:            Option[UserVault],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  modes:            SpectroscopyModesMatrix,
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedGroups:   View[Set[Group.Id]],
  readonly:         Boolean
) extends ReactFnProps(ObsTabContents.component):
  val focusedObs: Option[Observation.Id]        = focused.obsSet.map(_.head)
  val focusedTarget: Option[Target.Id]          = focused.target
  val focusedGroup: Option[Group.Id]            = focused.group
  val observations: UndoSetter[ObservationList] =
    programSummaries.zoom(ProgramSummaries.observations)
  val obsExecutions: ObservationExecutionMap    = programSummaries.get.obsExecutionPots
  val groupTimeRanges: GroupTimeRangeMap        = programSummaries.get.groupTimeRangePots
  val groups: UndoSetter[GroupTree]             = programSummaries.zoom(ProgramSummaries.groups)
  val targets: UndoSetter[TargetList]           = programSummaries.zoom(ProgramSummaries.targets)

object ObsTabContents extends TwoPanels:
  private type Props = ObsTabContents

  private def renderFn(
    props:        Props,
    selectedView: View[SelectedPanel],
    resize:       UseResizeDetectorReturn,
    deckShown:    View[DeckShown],
    ctx:          AppContext[IO]
  ): VdomNode = {

    def observationsTree() =
      if (deckShown.get === DeckShown.Shown) {
        ObsList(
          props.observations,
          props.obsExecutions,
          props.programSummaries,
          props.programId,
          props.focusedObs,
          props.focusedTarget,
          props.focusedGroup,
          selectedView.set(SelectedPanel.Summary),
          props.groups,
          props.expandedGroups,
          deckShown,
          props.readonly
        ): VdomNode
      } else
        <.div(ExploreStyles.TreeToolbar)(
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            disabled = false,
            icon = Icons.ArrowRightFromLine,
            clazz = ExploreStyles.ObsTreeHideShow,
            onClick = deckShown.mod(_.flip)
          ).mini.compact
        )

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Observations, selectedView, ctx)

    // def observationTable(): VdomNode = Tile(
    //   "observations".refined,
    //   "Observations Summary",
    //   backButton.some
    // )(renderInTitle =>
    //   ObsSummaryTable(
    //     props.vault.userId,
    //     props.programId,
    //     props.observations,
    //     props.obsExecutions,
    //     props.targets.get,
    //     renderInTitle
    //   )
    // // TODO: elevation view
    // )
    //
    def obsTiles(obsId: Observation.Id, resize: UseResizeDetectorReturn): VdomNode =
      val indexValue = Iso.id[ObservationList].index(obsId).andThen(KeyedIndexedList.value)

      props.observations.model
        .zoom(indexValue)
        .mapValue(obsView =>
          ObsTabTiles(
            props.vault,
            props.programId,
            props.modes,
            backButton,
            // FIXME Find a better mechanism for this.
            // Something like .mapValue but for UndoContext
            props.observations.zoom(indexValue.getOption.andThen(_.get), indexValue.modify),
            props.programSummaries
              .zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip),
            props.programSummaries.model.zoom(ProgramSummaries.obsAttachments),
            props.programSummaries.get,
            props.focusedTarget,
            props.searching,
            ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationsLayout),
            props.userPreferences.get.observationsTabLayout,
            resize,
            props.userPreferences.zoom(UserPreferences.globalPreferences),
            props.readonly
          ).withKey(s"${obsId.show}")
        )

    def groupTiles(groupId: Group.Id, resize: UseResizeDetectorReturn): VdomNode =
      ObsGroupTiles(
        props.vault.userId,
        groupId,
        props.groups,
        props.groupTimeRanges.getPot(groupId),
        resize,
        ExploreGridLayouts.sectionLayout(GridLayoutSection.GroupEditLayout),
        props.userPreferences.get.groupEditLayout,
        backButton
      )

    def rightSide(resize: UseResizeDetectorReturn): VdomNode =
      (props.focusedObs, props.focusedGroup) match {
        case (Some(obsId), _)   => obsTiles(obsId, resize)
        case (_, Some(groupId)) => groupTiles(groupId, resize)
        case _                  => <.div("OT") // observationTable()
      }

    makeOneOrTwoPanels(
      selectedView,
      observationsTree(),
      rightSide,
      RightSideCardinality.Multi,
      resize,
      ExploreStyles.ObsHiddenToolbar.when_(deckShown.get === DeckShown.Hidden)
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, _) => props.focusedObs) { (_, _, selected) => focusedObs =>
        (focusedObs, selected.get) match {
          case (Some(_), _)                 => selected.set(SelectedPanel.Editor)
          case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
          case _                            => Callback.empty
        }
      }
      // Measure its size
      .useResizeDetector()
      .useGlobalHotkeysWithDepsBy((props, ctx, _, _) =>
        (props.focusedObs,
         props.programSummaries.get.observations.values
           .map(_.id)
           .zipWithIndex
           .toList
           .map((id, idx) => id -> NonNegInt.unsafeFrom(idx)),
         props.readonly
        )
      ) { (props, ctx, _, _) => (obs, observationIds, readonly) =>
        import ctx.given

        val obsPos = observationIds.find(a => obs.forall(_ === a._1)).map(_._2)

        def callbacks: ShortcutCallbacks = {
          case CopyAlt1 | CopyAlt2 =>
            obs
              .map(id =>
                ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(ObsIdSet.one(id)))
                  .withToast(s"Copied obs $id")
              )
              .orUnit
              .runAsync

          case PasteAlt1 | PasteAlt2 =>
            ExploreClipboard.get
              .flatMap {
                case LocalClipboard.CopiedObservations(idSet) =>
                  idSet.idSet.toList
                    .traverse(oid =>
                      cloneObs(
                        props.programId,
                        oid,
                        NonNegInt.unsafeFrom(observationIds.length),
                        props.observations,
                        ctx
                      )
                    )
                    .void
                    .withToast(s"Duplicating obs ${idSet.idSet.mkString_(", ")}")
                case _                                        => IO.unit
              }
              .runAsync
              .unless_(readonly)

          case Down =>
            obsPos
              .filter(_.value < observationIds.length)
              .flatMap { p =>
                val next = if (props.focusedObs.isEmpty) 0 else p.value + 1
                observationIds.lift(next).map { (obsId, _) =>
                  ctx.setPageVia(
                    AppTab.Observations,
                    props.programId,
                    Focused.singleObs(obsId),
                    SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty

          case Up =>
            obsPos
              .filter(_.value > 0)
              .flatMap { p =>
                observationIds.lift(p.value - 1).map { (obsId, _) =>
                  ctx.setPageVia(
                    AppTab.Observations,
                    props.programId,
                    Focused.singleObs(obsId),
                    SetRouteVia.HistoryPush
                  )
                }
              }
              .getOrEmpty

          case GoToSummary =>
            ctx.setPageVia(
              AppTab.Observations,
              props.programId,
              Focused.None,
              SetRouteVia.HistoryPush
            )
        }
        UseHotkeysProps(((GoToSummary :: Up :: Down :: Nil) ::: (CopyKeys ::: PasteKeys)).toHotKeys,
                        callbacks
        )
      }
      .useStateView(DeckShown.Shown)
      .render((props, ctx, twoPanelState, resize, deckShown) =>
        renderFn(props, twoPanelState, resize, deckShown, ctx)
      )
