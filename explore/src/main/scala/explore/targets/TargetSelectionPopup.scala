// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.data.NonEmptyList
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Constants
import explore.model.TargetWithOptId
import explore.model.reusability._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import react.aladin._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class TargetSelectionPopup(
  trigger:          Reuse[Button],
  onSelected:       TargetWithOptId ==> Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  protected case class SelectedTarget(target: Target, sourceIndex: Int, resultIndex: Int)

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]

  implicit val reuseSelectedTarget: Reusability[SelectedTarget] = Reusability.derive

  implicit val reuseFiber: Reusability[FiberIO[Unit]] = Reusability.byRef

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateView("")
    // results
    .useStateWithReuse(
      SortedMap.empty[TargetSource[IO], (Int, NonEmptyList[TargetWithOptId])]
    )
    // searching
    .useState(false)
    // singleEffect
    .useSingleEffect
    // isOpen
    .useState(false)
    // selectedTarget
    .useState(none[SelectedTarget])
    // targetSources
    .useMemoBy((props, _, _, _, _, _, _) => props.ctx) { (_, _, _, _, _, _, _) => propsCtx =>
      implicit val ctx = propsCtx
      (Program.Id.parse("p-2").map(p => TargetSource.Program[IO](p)).toList ++
        TargetSource.forAllCatalogs[IO]).zipWithIndex
    }
    // aladinRef
    .useMemo(())(_ => Ref.toScalaComponent(Aladin.component))
    .renderWithReuse {
      (
        props,
        inputValue,
        results,
        searching,
        singleEffect,
        isOpen,
        selectedTarget,
        targetSources,
        aladinRef
      ) =>
        implicit val ctx = props.ctx

        val cleanResults = selectedTarget.setState(none) >> results.setState(SortedMap.empty)

        val cleanState =
          inputValue.set("") >> searching.setState(false) >> cleanResults

        def addResults(source: TargetSource[IO], index: Int)(
          targets:             List[TargetWithOptId]
        ): IO[Unit] =
          NonEmptyList
            .fromList(targets)
            .map[IO[Unit]](nel =>
              results.modStateAsync(r =>
                r.get(source).fold(r + (source -> ((index, nel)))) { case (i, ts) =>
                  r + (source -> ((i,
                                   // NonEmptyList doesn't have distinctBy
                                   NonEmptyList.fromListUnsafe(
                                     (ts.toList ++ nel.toList)
                                       .distinctBy(_ match {
                                         case TargetWithOptId(
                                               _,
                                               Target.Sidereal(name, _, _, catalogInfo, _)
                                             ) =>
                                           catalogInfo.map(_.id.value).getOrElse(name.value)
                                         case TargetWithOptId(
                                               _,
                                               Target.Nonsidereal(_, ephemerisKey, _, _)
                                             ) =>
                                           ephemerisKey.toString
                                       })
                                       .sortBy(_.target.name.value)
                                   )
                                  )
                  ))
                }
              )
            )
            .orEmpty

        def search(name: String): IO[Unit] =
          cleanResults.to[IO] >>
            NonEmptyString
              .from(name)
              .toOption
              .map(nonEmptyName =>
                searching.setStateAsync(true) >>
                  targetSources.value
                    .flatMap { case (source, index) =>
                      source.searches(nonEmptyName).map(_ >>= addResults(source, index))
                    }
                    .parSequence_
                    .guaranteeCase {
                      // If it gets canceled, it's because another search has started
                      case Outcome.Canceled() => IO.unit
                      case _                  => searching.setStateAsync(false)
                    }
              )
              .orEmpty

        React.Fragment(
          props.trigger.value(^.onClick --> isOpen.setState(true)),
          Modal(
            as = <.form,      // This lets us sumbit on enter
            actions = List(
              Button(size = Small, icon = true)(
                Icons.Close,
                "Cancel"
              )(^.tpe := "button", ^.key := "input-cancel")
            ),
            centered = false, // Works better on iOS
            open = isOpen.value,
            closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
            dimmer = Dimmer.Blurring,
            size = ModalSize.Small,
            onOpen = cleanState,
            onClose = singleEffect.cancel.runAsync >> isOpen.setState(false) >> cleanState,
            header = ModalHeader("Search Target"),
            content = ModalContent(
              <.span(ExploreStyles.TargetSearchTop)(
                <.span(ExploreStyles.TargetSearchInput)(
                  FormInputEV(
                    id = NonEmptyString("name"),
                    value = inputValue,
                    // TODO Investigate if we can replicate SUI's "input with icon" styles (which use <i>) but using <svg>,
                    // so that they work with fontawesome.
                    // icon = Icons.Search,
                    // iconPosition = IconPosition.Left,
                    onTextChange = t =>
                      inputValue.set(t) >>
                        singleEffect.submit(IO.sleep(700.milliseconds) >> search(t)).runAsync,
                    loading = searching.value
                  )
                    .withMods(^.placeholder := "Name", ^.autoFocus := true)
                ),
                <.div(ExploreStyles.TargetSearchPreview)(
                  selectedTarget.value
                    .map(_.target)
                    .collect { case Target.Sidereal(_, tracking, _, _, angSize) =>
                      (tracking.baseCoordinates, angSize)
                    }
                    .map { case (coordinates, angSize) =>
                      Aladin.component
                        .withRef(aladinRef)
                        .withKey(
                          selectedTarget.value.foldMap(t => s"${t.sourceIndex}-${t.resultIndex}")
                        )(
                          Aladin(
                            ExploreStyles.TargetSearchAladin,
                            showReticle = false,
                            showLayersControl = false,
                            target = Coordinates.fromHmsDms.reverseGet(coordinates),
                            fov = angSize
                              .map(_.majorAxis.toDoubleDegrees * Constants.AngleSizeFovFactor)
                              .getOrElse(Constants.InitialFov.toDoubleDegrees): Double,
                            showGotoControl = false
                          )
                        )
                    }
                    .whenDefined
                )
              ),
              SegmentGroup(raised = true, clazz = ExploreStyles.TargetSearchResults)(
                results.value.map { case (source, (sourceIndex, targets)) =>
                  Segment(
                    <.div(
                      Header(size = Small)(
                        s"${source.name} (${showCount(targets.length, "result")})"
                      ),
                      <.div(ExploreStyles.TargetSearchResultsSource)(
                        TargetSelectionTable(
                          targets.toList,
                          onSelected = props.onSelected.map(onSelected =>
                            t => onSelected(t) >> isOpen.setState(false) >> cleanState
                          ),
                          selectedIndex = selectedTarget.value
                            .filter(_.sourceIndex === sourceIndex)
                            .map(_.resultIndex),
                          onClick = Reuse.always { case (target: Target, index: Int) =>
                            selectedTarget.setState(
                              if (
                                selectedTarget.value.exists(st =>
                                  st.sourceIndex === sourceIndex && st.resultIndex === index
                                )
                              )
                                none
                              else
                                SelectedTarget(target, sourceIndex, index).some
                            )
                          }
                        )
                      )
                    )
                  )
                }.toTagMod
              ).when(results.value.nonEmpty)
            )
          )(
            ^.autoComplete.off,
            ^.onSubmit ==> (e => e.preventDefaultCB >> search(inputValue.get).runAsync)
          )
        )
    }
}
