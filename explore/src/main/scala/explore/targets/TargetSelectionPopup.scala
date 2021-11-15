// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.data.NonEmptyList
import cats.effect.FiberIO
import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import crystal.react.hooks._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.elements.header.Header

final case class TargetSelectionPopup(
  trigger:          Reuse[Button],
  onSelected:       Target ==> Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]

  implicit val reuseFiber: Reusability[FiberIO[Unit]] = Reusability.byRef

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateSnapshotWithReuse("")
    // results
    .useStateWithReuse(SortedMap.empty[TargetSource[IO], NonEmptyList[Target]])
    // searching
    .useState(none[FiberIO[Unit]])
    // timer
    .useDebouncedTimeout(700.milliseconds)
    // isOpen
    .useState(false)
    // targetSources
    .useMemoBy((props, _, _, _, _, _) => props.ctx) { (_, _, _, _, _, _) => propsCtx =>
      implicit val ctx = propsCtx
      Program.Id.parse("p-2").map(p => TargetSource.Program[IO](p)).toList ++
        TargetSource.forAllCatalogs[IO]
    }
    .renderWithReuse { (props, inputValue, results, searching, timer, isOpen, targetSources) =>
      implicit val ctx = props.ctx

      val cleanState = inputValue.setState("") >> results.setState(SortedMap.empty)

      def search(name: String): IO[Unit] =
        searching.value.map(_.cancel).orEmpty >>
          results.setStateAsync(SortedMap.empty) >>
          NonEmptyString
            .from(name)
            .toOption
            .map(nonEmptyName =>
              targetSources.value
                .traverse_(source =>
                  source.search(nonEmptyName) >>= (list =>
                    NonEmptyList
                      .fromList(list)
                      .map(nel => results.modStateAsync(_ + (source -> nel)): IO[Unit])
                      .orEmpty
                  )
                )
                .guarantee(
                  searching.setStateAsync(none) >> IO.println("Finished searching")
                )
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
            SegmentGroup(raised = true)(
              results.value.map { case (source, targets) =>
                Segment(
                  <.div(
                    Header(size = Small)(source.name),
                    <.div(ExploreStyles.SearchResults)(
                      TargetSelectionTable(
                        targets.toList,
                        onSelected = props.onSelected.map(onSelected =>
                          t => isOpen.setState(false) >> onSelected(t)
                        )
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
