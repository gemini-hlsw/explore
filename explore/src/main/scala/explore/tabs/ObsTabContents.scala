// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import explore.Icons
import explore.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.AppContext
import explore.model.ProgramSummaries
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.observationtree.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
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
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedGroups:   View[Set[Group.Id]]
) extends ReactFnProps(ObsTabContents.component):
  val focusedObs: Option[Observation.Id]                   = focused.obsSet.map(_.head)
  val focusedTarget: Option[Target.Id]                     = focused.target
  val obsAttachments: View[ObsAttachmentList]              =
    programSummaries.model.zoom(ProgramSummaries.obsAttachments)
  val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    programSummaries.get.obsAttachmentAssignments
  val observations: UndoSetter[ObservationList]            =
    programSummaries.zoom(ProgramSummaries.observations)
  val groups: UndoSetter[GroupList]                        = programSummaries.zoom(ProgramSummaries.groups)
  val targets: UndoSetter[TargetList]                      = programSummaries.zoom(ProgramSummaries.targets)

object ObsTabContents extends TwoPanels:
  private type Props = ObsTabContents

  private def renderFn(
    props:        Props,
    selectedView: View[SelectedPanel],
    resize:       UseResizeDetectorReturn,
    deckShown:    View[DeckShown],
    ctx:          AppContext[IO]
  ): VdomNode = {

    def observationsTree(observations: View[ObservationList]) =
      if (deckShown.get === DeckShown.Shown) {
        ObsList(
          props.observations,
          props.programSummaries,
          props.programId,
          props.focusedObs,
          props.focusedTarget,
          selectedView.set(SelectedPanel.Summary),
          props.groups.get,
          props.expandedGroups,
          deckShown
        ): VdomNode
      } else
        <.div(ExploreStyles.TreeToolbar)(
          Button(
            severity = Button.Severity.Secondary,
            outlined = true,
            disabled = false,
            icon = Icons.ArrowRightFromLine,
            onClick = deckShown.mod(_.flip)
          ).mini.compact
        )

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Observations, selectedView, ctx)

    def rightSide = (resize: UseResizeDetectorReturn) =>
      props.focusedObs.fold[VdomNode](
        Tile(
          "observations".refined,
          "Observations Summary",
          backButton.some
        )(renderInTitle =>
          ObsSummaryTable(
            props.userId,
            props.programId,
            props.observations.get,
            props.targets.get,
            renderInTitle
          )
        // TODO: elevation view
        )
      )(obsId =>
        val indexValue = Iso.id[ObservationList].index(obsId).andThen(KeyedIndexedList.value)

        props.observations.model
          .zoom(indexValue)
          .mapValue(obsView =>
            ObsTabTiles(
              props.vault,
              props.userId,
              props.programId,
              backButton,
              // FIXME Find a better mechanism for this.
              // Something like .mapValue but for UndoContext
              props.observations.zoom(indexValue.getOption.andThen(_.get), indexValue.modify),
              props.targets,
              // maybe we want constraintGroups, so we can get saner ids?
              props.programSummaries.get.constraintGroups.map(_._2).toSet,
              props.programSummaries.get.targetObservations,
              props.focusedTarget,
              props.searching,
              ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationsLayout),
              props.userPreferences.get.observationsTabLayout,
              resize,
              props.obsAttachments,
              props.obsAttachmentAssignments,
              props.userPreferences.zoom(UserPreferences.globalPreferences)
            ).withKey(s"${obsId.show}")
          )
      )

    makeOneOrTwoPanels(
      selectedView,
      observationsTree(props.observations.model),
      rightSide,
      RightSideCardinality.Multi,
      resize,
      ExploreStyles.ObsHiddenToolbar
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, panels) => (props.focusedObs, panels.reuseByValue)) {
        (_, _, _) => params =>
          val (focusedObs, selected) = params
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
         props.programSummaries.get.observations.values.map(_.id).zipWithIndex.toList
        )
      ) { (props, ctx, _, _) => (obs, observationIds) =>
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
            ExploreClipboard.get.flatMap {
              case LocalClipboard.CopiedObservations(idSet) =>
                idSet.idSet.toList
                  .traverse(oid =>
                    cloneObs(
                      props.programId,
                      oid,
                      observationIds.length,
                      props.observations,
                      ctx
                    )
                  )
                  .void
                  .withToast(s"Duplicating obs ${idSet.idSet.mkString_(", ")}")
              case _                                        => IO.unit
            }.runAsync

          case Down =>
            obsPos
              .filter(_ < observationIds.length)
              .flatMap { p =>
                val next = if (props.focusedObs.isEmpty) 0 else p + 1
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
              .filter(_ > 0)
              .flatMap { p =>
                observationIds.lift(p - 1).map { (obsId, _) =>
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
