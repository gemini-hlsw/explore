// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.data.NonEmptyList
import cats.effect.FiberIO
import cats.effect.IO
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.reusability._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.model.Program
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import react.aladin._
import react.common.ReactFnProps
import react.common.style.Css
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
  onSelected:       Target ==> Callback
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
    .useStateSnapshotWithReuse("")
    // results
    .useStateWithReuse(SortedMap.empty[TargetSource[IO], (Int, NonEmptyList[Target])])
    // searching
    .useState(none[FiberIO[Unit]])
    // timer
    .useDebouncedTimeout(700.milliseconds)
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
        timer,
        isOpen,
        selectedTarget,
        targetSources,
        aladinRef
      ) =>
        implicit val ctx = props.ctx

        println(selectedTarget.value)

        val cleanState = inputValue.setState("") >> selectedTarget.setState(none) >>
          results.setState(SortedMap.empty)

        def search(name: String): IO[Unit] =
          searching.value.map(_.cancel).orEmpty >>
            results.setStateAsync(SortedMap.empty) >>
            NonEmptyString
              .from(name)
              .toOption
              .map(nonEmptyName =>
                targetSources.value
                  .parTraverse_ { case (source, index) =>
                    source.search(nonEmptyName) >>= (list =>
                      NonEmptyList
                        .fromList(list)
                        .map(nel => results.modStateAsync(_ + (source -> ((index, nel)))): IO[Unit])
                        .orEmpty
                    )
                  }
                  .guarantee(searching.setStateAsync(none))
                  .start >>= (fiber => searching.setStateAsync(fiber.some))
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
            onClose = timer.cancel.runAsync >> isOpen.setState(false) >> cleanState,
            header = ModalHeader("Search Target"),
            content = ModalContent(
              <.span(^.display.flex)(
                FormInputEV(
                  id = NonEmptyString("name"),
                  value = inputValue,
                  // TODO Investigate if we can replicate SUI's "input with icon" styles (which use <i>) but using <svg>,
                  // so that they work with fontawesome.
                  // icon = Icons.Search,
                  // iconPosition = IconPosition.Left,
                  onTextChange = t => inputValue.setState(t) >> timer.submit(search(t)).runAsync,
                  loading = searching.value.nonEmpty
                )
                  .withMods(^.placeholder := "Name", ^.autoFocus := true),
                <.div(^.width := "200px", ^.height := "200px")(
                  selectedTarget.value
                    .map(_.target)
                    .collect { case SiderealTarget(_, tracking, _) =>
                      tracking.baseCoordinates
                    }
                    .map(coordinates =>
                      Aladin.component
                        .withRef(aladinRef)
                        .withKey(
                          selectedTarget.value.foldMap(t => s"${t.sourceIndex}-${t.resultIndex}")
                        )(
                          Aladin(
                            Css("aladin-search-target"),
                            showReticle = false,
                            showLayersControl = false,
                            target = Coordinates.fromHmsDms.reverseGet(coordinates),
                            fov = 0.25,
                            showGotoControl = false
                          )
                        )
                    )
                    .whenDefined
                )
              ),
              SegmentGroup(raised = true, clazz = ExploreStyles.SearchResults)(
                results.value.map { case (source, (sourceIndex, targets)) =>
                  Segment(
                    <.div(
                      Header(size = Small)(
                        s"${source.name} (${showCount(targets.length, "result")})"
                      ),
                      <.div(ExploreStyles.SearchResultsSource)(
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
            ^.onSubmit ==> (e => e.preventDefaultCB >> search(inputValue.value).runAsync)
          )
        )
    }
}
