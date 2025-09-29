// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Eq
import cats.Order.*
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.EmptyOpportunityTarget
import explore.model.EmptySiderealTarget
import explore.model.PopupState
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.catalog.AngularSize
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.util.NewBoolean
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.refined.*
import lucuma.schemas.model.TargetWithOptId
import lucuma.ui.aladin.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

case class TargetSelectionPopup(
  title:               String,
  targetSources:       NonEmptyList[TargetSource[IO]],
  selectExistingLabel: String,
  selectExistingIcon:  FontAwesomeIcon,
  selectNewLabel:      String,
  selectNewIcon:       FontAwesomeIcon,
  trigger:             Button,
  onSelected:          TargetWithOptId => Callback,
  onCancel:            Callback = Callback.empty,
  initialSearch:       Option[NonEmptyString] = None,
  showCreateEmpty:     Boolean = true
) extends ReactFnProps(TargetSelectionPopup.component)

object SearchingState extends NewBoolean { inline def Searching = True; inline def Idle = False }

type SearchingState = SearchingState.Type

object TargetSelectionPopup:
  private type Props = TargetSelectionPopup

  private case class Result(target: TargetSearchResult, priority: Int) derives Eq

  private case class SelectedTarget(
    target:      Target,
    source:      TargetSource[IO],
    resultIndex: Int,
    angularSize: Option[AngularSize]
  ) derives Eq

  private given Reusability[SelectedTarget] = Reusability.byEq
  private given Reusability[AladinOptions]  = reusability.all

  private def addResults(
    source:         TargetSource[IO],
    priority:       Int,
    results:        View[SortedMap[TargetSource[IO], NonEmptyList[Result]]],
    selectedTarget: View[Option[SelectedTarget]]
  )(
    targets:        List[TargetSearchResult]
  )(using Logger[IO]): IO[Unit] =
    NonEmptyList
      .fromList(targets.map(t => Result(t, priority)))
      .map[IO[Unit]](nel =>
        results.async.mod(r =>
          r.get(source).fold(r + (source -> nel)) { case rs =>
            r + (source ->
              NonEmptyList
                .fromListUnsafe {
                  // Remove duplicates, keeping the one with the highest priority (lowest value).
                  rs.filterNot(r =>
                    nel.exists(r0 => r.target === r0.target && r0.priority < r.priority)
                  ) ++
                    nel.filterNot(r0 =>
                      rs.exists(r => r.target === r0.target && r0.priority > r.priority)
                    )
                }
                .sortBy(r => (r.priority, r.target.target.name.value)))
          }
        ) >> selectedTarget.async.mod(s =>
          s.orElse(
            SelectedTarget(
              nel.head.target.target,
              source,
              0,
              nel.head.target.angularSize
            ).some
          )
        )
      )
      .orEmpty

  private def search(
    name:           String,
    targetSources:  NonEmptyList[TargetSource[IO]],
    results:        View[SortedMap[TargetSource[IO], NonEmptyList[Result]]],
    selectedTarget: View[Option[SelectedTarget]],
    searching:      View[SearchingState]
  )(using Logger[IO]): IO[Unit] =
    NonEmptyString
      .from(name)
      .toOption
      .map(nonEmptyName =>
        searching.async.set(SearchingState.Searching) >>
          targetSources.toList
            .flatMap(source =>
              source.searches(nonEmptyName).zipWithIndex.map { case (search, priority) =>
                search >>= addResults(source, priority, results, selectedTarget)
              }
            )
            .parSequence_
            .guaranteeCase {
              // If it gets canceled, it's because another search has started
              case Outcome.Canceled() => IO.unit
              case _                  => searching.async.set(SearchingState.Idle)
            }
      )
      .orEmpty

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // inputValue
    .useStateViewBy((props, _) => props.initialSearch.map(_.value).orEmpty)
    // results
    .useStateView(SortedMap.empty[TargetSource[IO], NonEmptyList[Result]])
    // searching
    .useStateView(SearchingState.Idle)
    // singleEffect
    .useSingleEffect
    // isOpen
    .useState(PopupState.Closed)
    // selectedTarget
    .useStateView(none[SelectedTarget])
    // aladinRef
    .useState(none[Aladin])
    // re render when selected changes
    .useEffectWithDepsBy((_, _, _, _, _, _, _, selectedTarget, _) => selectedTarget.get)(
      (_, _, _, _, _, _, _, _, aladinRef) =>
        sel =>
          aladinRef.value
            .map(a =>
              sel
                .collect { case SelectedTarget(Target.Sidereal(_, tracking, _, _), _, _, _) =>
                  tracking.baseCoordinates
                }
                .map(a.gotoRaDecCB)
                .orEmpty
            )
            .orEmpty
            // We need to do this callback delayed or it miss calculates aladin div size
            .delayMs(10)
    )
    .render {
      (
        props,
        ctx,
        inputValue,
        results,
        searching,
        singleEffect,
        isOpen,
        selectedTarget,
        aladinRef
      ) =>
        import ctx.given

        val cleanResults = selectedTarget.set(none) >> results.set(SortedMap.empty)

        val cleanState =
          inputValue.set("") >> searching.set(SearchingState.Idle) >> cleanResults

        def searchName(name: String): IO[Unit] =
          cleanResults.toAsync >>
            search(name, props.targetSources, results, selectedTarget, searching)

        val onOpen: Callback =
          cleanState >>
            isOpen.setState(PopupState.Open) >>
            props.initialSearch
              .map(name =>
                inputValue.set(name.value) >>
                  singleEffect
                    .submit(
                      search(name.value, props.targetSources, results, selectedTarget, searching)
                    )
                    .runAsync
              )
              .orEmpty

        React.Fragment(
          props.trigger.copy(onClick = props.trigger.onClick >> onOpen),
          Dialog(
            closable = false,
            clazz = ExploreStyles.TargetSearchForm |+| LucumaPrimeStyles.Dialog.Large,
            contentClass = ExploreStyles.TargetSearchContent,
            footer = <.div(
              Button(
                label = "Close",
                icon = Icons.Close,
                severity = Button.Severity.Danger,
                onClick = isOpen.setState(PopupState.Closed) >> props.onCancel
              ).small,
              <.span(
                Button(
                  label = "Create Empty Sidereal Target",
                  icon = Icons.New,
                  severity = Button.Severity.Success,
                  onClick = {
                    val target = NonEmptyString
                      .from(inputValue.get)
                      .toOption
                      .fold(EmptySiderealTarget)(s => EmptySiderealTarget.copy(name = s))
                    props
                      .onSelected(TargetWithOptId(none, target))
                      .flatTap(_ => isOpen.setState(PopupState.Closed))
                  }
                ).small.when(props.showCreateEmpty),
                Button(
                  label = "Create Empty Target of Opportunity",
                  icon = Icons.New,
                  severity = Button.Severity.Success,
                  onClick = {
                    val target = NonEmptyString
                      .from(inputValue.get)
                      .toOption
                      .fold(EmptyOpportunityTarget)(s => EmptyOpportunityTarget.copy(name = s))
                    props
                      .onSelected(TargetWithOptId(none, target))
                      .flatTap(_ => isOpen.setState(PopupState.Closed))
                  }
                ).small.when(props.showCreateEmpty)
              )
            ),
            position = DialogPosition.Top,
            visible = isOpen.value.value,
            dismissableMask = true,
            onHide = singleEffect.cancel.runAsync >> isOpen
              .setState(PopupState.Closed) >> cleanState >> props.onCancel,
            header = props.title
          )(
            React.Fragment(
              <.span(ExploreStyles.TargetSearchTop)(
                <.form(ExploreStyles.TargetSearchInput)(
                  FormInputTextView(
                    id = "target-search-name".refined,
                    placeholder = "Name",
                    value = inputValue,
                    preAddons =
                      List(if (searching.get.value) Icons.Spinner.withSpin(true) else Icons.Search),
                    onTextChange = (t: String) =>
                      inputValue.set(t) >>
                        singleEffect
                          .submit(
                            IO.sleep(700.milliseconds) >> searchName(t)
                          )
                          .runAsync
                  ).withMods(^.autoFocus := true)
                )(
                  ^.autoComplete.off,
                  ^.onSubmit ==> (e =>
                    e.preventDefaultCB >>
                      singleEffect
                        .submit(searchName(inputValue.get))
                        .runAsync
                        .whenA(searching.get == SearchingState.Searching)
                  )
                )
              ),
              <.div(ExploreStyles.TargetSearchPreview)(
                aladinRef.value.map(AladinZoomControl(_, factor = 1.5)),
                selectedTarget.get
                  .collect { case SelectedTarget(Target.Sidereal(_, tracking, _, _), _, _, _) =>
                    tracking.baseCoordinates
                  }
                  .map[VdomNode] { case coordinates =>
                    ReactAladin(
                      ExploreStyles.TargetSearchAladin, // required placeholder
                      AladinOptions(
                        showReticle = false,
                        showLayersControl = false,
                        target = Coordinates.fromHmsDms.reverseGet(coordinates),
                        fov = Constants.PreviewFov,
                        fullScreen = false,
                        showZoomControl = false,
                        showFullscreenControl = false,
                        showGotoControl = false
                      ),
                      customize = v => aladinRef.setState(v.some)
                    )(^.key := selectedTarget.get.foldMap(t => s"${t.source}-${t.resultIndex}"))
                  }
                  .getOrElse(<.div(ExploreStyles.TargetSearchPreviewPlaceholder, "Preview"))
              ),
              results.get.map { case (source, sourceResults) =>
                val fmtdCount = s"(${showCount(sourceResults.length, "result")})"

                val header =
                  if (source.existing)
                    s"Link an existing target $fmtdCount"
                  else
                    s"Add a new target from ${source.name} (${showCount(sourceResults.length, "result")})"

                React.Fragment.withKey(source.name)(
                  <.div(ExploreStyles.SmallHeader, header),
                  <.div(ExploreStyles.TargetSearchResults)(
                    TargetSelectionTable(
                      source,
                      sourceResults.toList.map(_.target),
                      props.selectExistingLabel,
                      props.selectExistingIcon,
                      props.selectNewLabel,
                      props.selectNewIcon,
                      onSelected = t =>
                        props.onSelected(t.targetWithOptId) >>
                          isOpen.setState(PopupState.Closed) >>
                          cleanState,
                      selectedIndex = selectedTarget.get
                        .filter(_.source === source)
                        .map(_.resultIndex),
                      onClick = (result, index) =>
                        selectedTarget.set(
                          if (
                            selectedTarget.get
                              .exists(st => st.source === source && st.resultIndex === index)
                          )
                            none
                          else
                            SelectedTarget(
                              result.target,
                              source,
                              index,
                              result.angularSize
                            ).some
                        )
                    )
                  )
                )
              }.toVdomArray
            )
          )
        )
    }
