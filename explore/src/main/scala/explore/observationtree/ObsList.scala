// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.given
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.GroupQueries
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupElement
import explore.model.GroupList
import explore.model.Grouping
import explore.model.ObservationExecutionMap
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.tabs.DeckShown
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node
import lucuma.typed.primereact.treeTreeMod.TreeNodeTemplateOptions
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Iso
import org.scalajs.dom
import org.scalajs.dom.Element
import queries.schemas.odb.ObsQueries

import scala.annotation.tailrec
import scala.scalajs.js

import ObsQueries.*

case class ObsList(
  observations:      UndoSetter[ObservationList],
  obsExecutionTimes: ObservationExecutionMap,
  undoer:            Undoer,
  programId:         Program.Id,
  focusedObs:        Option[Observation.Id],
  focusedTarget:     Option[Target.Id],
  setSummaryPanel:   Callback,
  groups:            UndoSetter[GroupList],
  expandedGroups:    View[Set[Group.Id]],
  deckShown:         View[DeckShown],
  readonly:          Boolean
) extends ReactFnProps(ObsList.component)

object ObsList:
  private type Props = ObsList

  private given Reusability[GroupElement] = Reusability.byEq

  /**
   * Iso to go between Group.Id and Tree.Id
   */
  private val groupTreeIdLens: Iso[Set[Group.Id], Set[Tree.Id]] =
    Iso[Set[Group.Id], Set[Tree.Id]](_.map(k => Tree.Id(k.toString)))(a =>
      a.flatMap(v => Group.Id.parse(v.value))
    )

  private def scrollIfNeeded(targetObs: Observation.Id) =
    Callback {
      Option(dom.document.getElementById(s"obs-list-${targetObs.toString}"))
        .filterNot(js.isUndefined)
        .map { obsListElement =>
          val rect = obsListElement.getBoundingClientRect()
          if (rect.top < 0) obsListElement.scrollIntoView()
          if (rect.bottom > dom.window.innerHeight) obsListElement.scrollIntoView(false)
        }
    }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Saved index into the observation list
      .useState(none[Int])
      .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.observations.get)) {
        (props, ctx, optIndex) => params =>
          val (focusedObs, obsList) = params

          focusedObs.fold(optIndex.setState(none)) { obsId =>
            // there is a focused obsId, look for it in the list
            val foundIdx = obsList.getIndex(obsId)
            (optIndex.value, foundIdx) match {
              case (_, Some(fidx))    =>
                optIndex.setState(fidx.some) // focused obs is in list
              case (None, None)       =>
                setObs(props.programId, none, ctx) >> optIndex.setState(none)
              case (Some(oidx), None) =>
                // focused obs no longer exists, but we have a previous index.
                val newIdx = math.min(oidx, obsList.length - 1)
                obsList.toList
                  .get(newIdx.toLong)
                  .fold(
                    optIndex.setState(none) >> setObs(props.programId, none, ctx)
                  )(obsSumm =>
                    optIndex.setState(newIdx.some) >> setObs(props.programId, obsSumm.id.some, ctx)
                  )
            }
          }
      }
      // adding new observation
      .useStateView(AddingObservation(false))
      // treeNodes
      .useMemoBy((props, _, _, _) =>
        (props.observations.model.reuseByValue, props.groups.model.reuseByValue)
      )((props, _, _, _) =>
        (obs, groups) => (obs.value, groups.value).tupled.as(ObsNode.obsGroupNodeIso)
      )
      // Scroll to newly created/selected observation
      .useEffectWithDepsBy((props, _, _, _, _) => props.focusedObs)((_, _, _, _, _) =>
        focusedObs => focusedObs.map(scrollIfNeeded).getOrEmpty
      )
      // Open the group (and all super-groups) of the focused observation
      .useEffectWithDepsBy((props, _, _, _, _) => (props.focusedObs, props.groups.get))(
        (props, _, _, _, _) =>
          case (None, _)             => Callback.empty
          case (Some(obsId), groups) =>
            @tailrec
            def findParentGroups(
              groupElementId: Either[Observation.Id, Group.Id],
              acc:            Set[Group.Id]
            ): Set[Group.Id] = {
              val parentGroup = groups.find(
                GroupElement.grouping
                  .exist(_.elements.exists(_.bimap(_.id, _.id) === groupElementId))
              )

              parentGroup match
                case None                                                => acc
                case Some(GroupElement(Left(_), _))                      => acc
                // We've found the 'root' group, so we're done
                case Some(GroupElement(Right(grouping), None))           => acc + grouping.id
                case Some(GroupElement(Right(grouping), Some(parentId))) =>
                  findParentGroups(parentId.asRight, acc ++ Set(parentId, grouping.id))
            }

            val groupsToAddFocus =
              findParentGroups(obsId.asLeft, Set.empty)

            Callback.when(groupsToAddFocus.nonEmpty)(
              props.expandedGroups.mod(_ ++ groupsToAddFocus)
            )
      )
      .render { (props, ctx, _, adding, treeNodes) =>
        import ctx.given

        val expandedGroups = props.expandedGroups.as(groupTreeIdLens)

        def onDragDrop(e: Tree.DragDropEvent[ObsNode]): Callback =
          if (e.dropNode.exists(_.data.isObs)) Callback.empty
          else {
            val dragNodeId = e.dragNode.data.id
            val dropNodeId = e.dropNode
              .flatMap(_.data.value.fold(_.groupId, _.id.some))

            val dropIndex =
              NonNegShort.from(e.dropIndex.toShort).getOrElse(NonNegShort.unsafeFrom(0))

            dragNodeId
              .fold(
                obsId =>
                  ObsQueries
                    .moveObservation[IO](obsId, dropNodeId, dropIndex.some),
                groupId => GroupQueries.moveGroup[IO](groupId, dropNodeId, dropIndex.some)
              )
              .runAsync *>
              treeNodes.value.set(e.value) *>
              // Open the group we moved to
              dropNodeId.map(id => props.expandedGroups.mod(_ + id)).getOrEmpty
          }

        def renderItem(node: ObsNode, options: TreeNodeTemplateOptions) =
          node match
            case ObsNode.Obs(obs)   =>
              val id       = obs.id
              val selected = props.focusedObs.exists(_ === id)
              <.a(
                ^.id        := s"obs-list-${id.toString}",
                ^.href      := ctx.pageUrl(
                  AppTab.Observations,
                  props.programId,
                  Focused.singleObs(id, props.focusedTarget)
                ),
                // Disable link dragging to enable tree node dragging
                ^.draggable := false,
                ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                ^.onClick ==> linkOverride(
                  setObs(props.programId, id.some, ctx)
                )
              )(
                ObsBadge(
                  obs,
                  props.obsExecutionTimes.getPot(id).map(_.executionTime),
                  ObsBadge.Layout.ObservationsTab,
                  selected = selected,
                  setStatusCB = obsEditStatus(id)
                    .set(props.observations)
                    .compose((_: ObsStatus).some)
                    .some,
                  setActiveStatusCB = obsActiveStatus(id)
                    .set(props.observations)
                    .compose((_: ObsActiveStatus).some)
                    .some,
                  setSubtitleCB = obsEditSubtitle(id)
                    .set(props.observations)
                    .compose((_: Option[NonEmptyString]).some)
                    .some,
                  deleteCB = obsExistence(
                    id,
                    o => setObs(props.programId, o.some, ctx)
                  )
                    .mod(props.observations)(obsListMod.delete)
                    .showToastCB(s"Deleted obs ${id.show}")
                    .some,
                  cloneCB = cloneObs(
                    props.programId,
                    id,
                    props.observations.get.length,
                    props.observations,
                    ctx,
                    adding.async.set(AddingObservation(true)),
                    adding.async.set(AddingObservation(false))
                  )
                    .withToast(s"Duplicating obs ${id}")
                    .runAsync
                    .some,
                  readonly = props.readonly
                )
              )
            case ObsNode.And(group) => renderGroup("AND", group)
            case ObsNode.Or(group)  => renderGroup("OR", group)

        def renderGroup(title: String, group: Grouping) =
          <.span(title,
                 ExploreStyles.ObsTreeGroupLeaf,
                 group.name.map(<.em(_, ^.marginLeft := "8px")),
                 ^.title := group.id.show
          )

        val tree =
          if (props.deckShown.get === DeckShown.Shown) {
            React.Fragment(
              <.div(ExploreStyles.TreeToolbar)(
                if (props.readonly) EmptyVdom
                else
                  React.Fragment(
                    Button(
                      severity = Button.Severity.Success,
                      icon = Icons.New,
                      label = "Obs",
                      disabled = adding.get.value,
                      loading = adding.get.value,
                      onClick = insertObs(
                        props.programId,
                        props.observations.get.length,
                        props.observations,
                        adding,
                        ctx
                      ).runAsync
                    ).mini.compact,
                    Button(
                      severity = Button.Severity.Success,
                      icon = Icons.New,
                      label = "Group",
                      disabled = adding.get.value,
                      loading = adding.get.value,
                      onClick = insertGroup(
                        props.programId,
                        props.groups,
                        adding,
                        ctx
                      ).runAsync
                    ).mini.compact
                  ),
                <.div(
                  ExploreStyles.ObsTreeButtons,
                  Button(
                    severity = Button.Severity.Secondary,
                    outlined = true,
                    disabled = false,
                    icon = Icons.ArrowLeftFromLine,
                    clazz = ExploreStyles.ObsTreeHideShow,
                    onClick = props.deckShown.mod(_.flip)
                  ).mini.compact,
                  if (props.readonly) EmptyVdom
                  else
                    UndoButtons(props.undoer, size = PlSize.Mini, disabled = adding.get.value)
                )
              ),
              <.div(
                Button(
                  severity = Button.Severity.Secondary,
                  icon = Icons.ListIcon,
                  label = "Observations Summary",
                  onClick = setObs(props.programId, none, ctx) >> props.setSummaryPanel,
                  clazz = ExploreStyles.ButtonSummary
                )
              ),
              <.div(
                ^.overflow := "auto",
                Tree(
                  treeNodes.get,
                  renderItem,
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set,
                  dragDropScope = if (props.readonly) js.undefined else "obs-tree",
                  onDragDrop = if (props.readonly) js.undefined else onDragDrop
                )
              )
            )
          } else EmptyVdom

        <.div(ExploreStyles.ObsTreeWrapper)(tree)
      }
