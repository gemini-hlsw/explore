package explore.targets

import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactFnProps
import lucuma.core.model.SiderealTarget
import react.semanticui.modules.modal.Modal
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import explore.Icons
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import explore.components.ui.ExploreStyles
import lucuma.ui.forms.FormInputEV
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ui.reusability._
import explore.model.reusability._
import cats.data.NonEmptyList
import react.semanticui.elements.segment.Segment
import scala.collection.immutable.SortedMap
import cats.Order._
import lucuma.core.util.Enumerated
import cats.effect.IO
import explore.implicits._
import explore.common.SimbadSearch
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.effect.Sync
import cats.effect.FiberIO

final case class TargetSelectionPopup(
  trigger:          Reuse[VdomNode],
  onComplete:       SiderealTarget ==> Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]

  protected sealed trait TargetSource extends Product with Serializable {
    def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]]
  }
  protected object TargetSource {
    case object Program extends TargetSource {
      override def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]] =
        Sync[F].delay(List.empty)
    }
    case object Simbad  extends TargetSource {
      override def search[F[_]: Async: Logger](name: NonEmptyString): F[List[SiderealTarget]] =
        // IO.println(s"Searching for $name in Simbad") >>
        SimbadSearch
          .search[F](name)
          .map(_.toList)
    }

    implicit val enumTargetSource: Enumerated[TargetSource] = Enumerated.of(Program, Simbad)
  }

  implicit val reuseFiber: Reusability[FiberIO[Unit]] = Reusability.byRef

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateSnapshotWithReuse("")
    // results
    .useStateWithReuse(SortedMap.empty[TargetSource, NonEmptyList[SiderealTarget]])
    // searching
    .useState(none[FiberIO[Unit]])
    .renderWithReuse { (props, inputValue, results, searching) =>
      implicit val ctx = props.ctx

      val cleanState = inputValue.setState("") >> results.setState(SortedMap.empty)

      def search(name: String): IO[Unit] =
        searching.value.map(_.cancel).orEmpty >>
          // TODO setStateIn[F] for hooks in crystal
          (results.setState(SortedMap.empty): Callback).to[IO] >>
          NonEmptyString
            .from(name)
            .toOption
            .map(nonEmptyName =>
              Enumerated[TargetSource].all
                .traverse_(source =>
                  source.search[IO](nonEmptyName) >>= (list =>
                    NonEmptyList
                      .fromList(list)
                      .map(nel => results.modState(_ + (source -> nel)): Callback)
                      .orEmpty
                      .to[IO]
                  )
                )
                .guarantee(
                  (searching.setState(none) >> Callback.log("Finished searching")).to[IO]
                )
                .start >>= (fiber => (searching.setState(fiber.some): Callback).to[IO])
            )
            .orEmpty
      // .runAsync

      Modal(
        as = <.form,      // This lets us sumbit on enter
        actions = List(
          Button(size = Small, icon = true)(
            Icons.Close,
            "Cancel"
          )(^.key := "input-cancel")
        ),
        centered = false, // Works better on iOS
        trigger = props.trigger.value,
        closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        dimmer = Dimmer.Blurring,
        size = ModalSize.Small,
        onClose = cleanState,
        header = ModalHeader("Search Target"),
        content = ModalContent(
          FormInputEV(
            id = NonEmptyString("name"),
            value = inputValue,
            // icon = Icons.Search,
            // iconPosition = IconPosition.Left,
            onTextChange = t => inputValue.setState(t) >> search(t).runAsync,
            loading = searching.value.nonEmpty
          )
            .withMods(^.placeholder := "Name", ^.autoFocus := true),
          Segment(
            <.div(
              results.value.map { case (source, targets) =>
                Segment(
                  <.div(
                    <.p(Enumerated[TargetSource].tag(source)),
                    targets.toList.map { target =>
                      target.toString
                    }.toTagMod
                  )
                )
              }.toTagMod
            )
          ).when(results.value.nonEmpty)
        )
      )

    }
}
