// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupElement
import explore.model.GroupList
import explore.model.Grouping
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.observationtree.ObsBadge
import explore.tabs.DeckShown
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.typed.primereact.treeTreeMod.TreeNodeTemplateOptions
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Lens
import org.scalajs.dom
import org.scalajs.dom.Element
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Tree

import scala.annotation.tailrec
import scala.scalajs.js

import ObsQueries.*

case class ObsList(
  obsUndoCtx:      UndoContext[ObservationList],
  programId:       Program.Id,
  focusedObs:      Option[Observation.Id],
  focusedTarget:   Option[Target.Id],
  setSummaryPanel: Callback,
  groups:          GroupList,
  expandedGroups:  View[Set[Group.Id]],
  deckShown:       View[DeckShown]
) extends ReactFnProps(ObsList.component):
  val observations: ObservationList = obsUndoCtx.model.get

object ObsList:
  private type Props = ObsList

  private given Reusability[GroupElement] = Reusability.byEq

  private val groupTreeIdLens: Lens[Set[Group.Id], Set[Tree.Id]] =
    Lens[Set[Group.Id], Set[Tree.Id]](_.map(k => Tree.Id(k.toString)))(a =>
      _ => a.flatMap(v => Group.Id.parse(v.value))
    )

  private def scrollIfNeeded(targetObs: Observation.Id) =
    Callback {
      // 'getElementById' does not return any optional type, so coerce to js.UndefOr
      val target: js.UndefOr[Element] =
        dom.document.getElementById(s"obs-list-${targetObs.toString}")
      target.foreach { t =>
        val rect = t.getBoundingClientRect()
        if (rect.top < 0) t.scrollIntoView()
        if (rect.bottom > dom.window.innerHeight) t.scrollIntoView(false)
      }
    }

  private def insertObs(
    programId: Program.Id,
    pos:       Int,
    undoCtx:   UndoContext[ObservationList],
    adding:    View[Boolean],
    ctx:       AppContext[IO]
  ): IO[Unit] =
    import ctx.given

    adding.async.set(true) >>
      createObservation[IO](programId)
        .flatMap { obs =>
          (obsExistence(programId, obs.id, o => setObs(programId, o.some, ctx))
            .mod(undoCtx)(obsListMod.upsert(obs, pos)) <* scrollIfNeeded(obs.id))
            .to[IO]
        }
        .guarantee(adding.async.set(false))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Saved index into the observation list
      .useState(none[Int])
      .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.observations)) {
        (props, ctx, optIndex) => params =>
          import ctx.given

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
      .useStateView(false)
      .useMemoBy((props, _, _, _) => (props.observations, props.groups))((_, _, _, _) =>
        ObsNode.fromList
      )
      .useEffectWithDepsBy((props, _, _, _, _) => (props.focusedObs, props.groups))(
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

        val observations = props.observations.toList

        val expandedGroups = props.expandedGroups.zoom(groupTreeIdLens)

        def renderItem(node: ObsNode, options: TreeNodeTemplateOptions) =
          node match
            case ObsNode.Obs(obs)   =>
              val id       = obs.id
              val selected = props.focusedObs.exists(_ === id)
              <.a(
                ^.id   := s"obs-list-${id.toString}",
                ^.href := ctx.pageUrl(
                  AppTab.Observations,
                  props.programId,
                  Focused.singleObs(id, props.focusedTarget)
                ),
                ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                ^.onClick ==> linkOverride(
                  setObs(props.programId, id.some, ctx)
                )
              )(
                ObsBadge(
                  obs,
                  ObsBadge.Layout.ObservationsTab,
                  selected = selected,
                  setStatusCB = (obsEditStatus(props.programId, id)
                    .set(props.obsUndoCtx) _).compose((_: ObsStatus).some).some,
                  setActiveStatusCB = (obsActiveStatus(props.programId, id)
                    .set(props.obsUndoCtx) _).compose((_: ObsActiveStatus).some).some,
                  setSubtitleCB = (obsEditSubtitle(props.programId, id)
                    .set(props.obsUndoCtx) _).compose((_: Option[NonEmptyString]).some).some,
                  deleteCB = obsExistence(
                    props.programId,
                    id,
                    o => setObs(props.programId, o.some, ctx)
                  )
                    .mod(props.obsUndoCtx)(obsListMod.delete)
                    .showToastCB(ctx)(s"Deleted obs ${id.show}")
                    .some,
                  cloneCB = cloneObs(
                    props.programId,
                    id,
                    observations.length,
                    props.obsUndoCtx,
                    ctx,
                    adding.async.set(true),
                    adding.async.set(false)
                  )
                    .withToast(s"Duplicating obs ${id}")
                    .runAsync
                    .some
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
                Button(
                  severity = Button.Severity.Success,
                  icon = Icons.New,
                  label = "Obs",
                  disabled = adding.get,
                  loading = adding.get,
                  onClick = insertObs(
                    props.programId,
                    observations.length,
                    props.obsUndoCtx,
                    adding,
                    ctx
                  ).runAsync
                ).mini.compact,
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
                  UndoButtons(props.obsUndoCtx, size = PlSize.Mini, disabled = adding.get)
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
                  treeNodes,
                  renderItem,
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set
                )
              )
            )
          } else EmptyVdom

        <.div(ExploreStyles.ObsTreeWrapper)(tree)
      }
