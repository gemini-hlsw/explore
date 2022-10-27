// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.*
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import fs2.*
import fs2.text
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.catalog.csv.TargetImport
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import org.scalajs.dom.{File => DOMFile}
import org.typelevel.log4cats.Logger
import queries.common.TargetQueriesGQL.*
import queries.schemas.odb.ODBConversions.*
import react.common.ReactFnProps
import react.fa.IconSize
import react.primereact.Button
import react.primereact.Dialog
import react.primereact.DialogPosition
import react.primereact.ProgressSpinner

case class TargetImportPopup(
  programId: Program.Id,
  files:     View[List[DOMFile]]
) extends ReactFnProps(TargetImportPopup.component)

object TargetImportPopup:
  private type Props = TargetImportPopup

  private case class State(
    loaded:       List[Target],
    current:      Option[Target],
    genericError: Option[String],
    targetErrors: Int,
    done:         Boolean
  )

  private object State {
    val loaded: Lens[State, List[Target]]         = Focus[State](_.loaded)
    val genericError: Lens[State, Option[String]] = Focus[State](_.genericError)
    val targetErrors: Lens[State, Int]            = Focus[State](_.targetErrors)
    val done: Lens[State, Boolean]                = Focus[State](_.done)
    val Default                                   = State(Nil, none, none, 0, false)
  }

  private given Reusability[DOMFile] = Reusability.by(_.name)

  private def importTargets[F[_]: Concurrent: Logger](
    programId:   Program.Id,
    s:           Stream[F, Byte],
    stateUpdate: (State => State) => F[Unit]
  )(using TransactionalClient[F, ObservationDB]) =
    s
      .through(text.utf8.decode)
      .through(TargetImport.csv2targets)
      .evalMap {
        case Left(a)       =>
          stateUpdate(State.targetErrors.modify(_ + 1))
        case Right(target) =>
          CreateTargetMutation
            .execute(target.toCreateTargetInput(programId))
            .map(_.createTarget.target.id.some)
            .flatTap(_ =>
              stateUpdate(l => l.copy(current = target.some, loaded = (target :: l.loaded).reverse))
            )
            .void
      }
      .handleErrorWith(e =>
        Stream.eval(stateUpdate(State.genericError.replace(e.getMessage().some)))
      )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(State.Default)
      .useEffectWithDepsBy((props, _, _) => props.files.get) { (props, ctx, state) => files =>
        import ctx.given

        state.setState(State.Default).to[IO] *>
          files
            .traverse(f =>
              importTargets[IO](props.programId,
                                dom.readReadableStream(IO(f.stream())),
                                state.modState(_).to[IO]
              ).compile.toList
            )
            .flatTap(_ => state.modState(State.done.replace(true)).to[IO])
            .void
            .whenA(files.nonEmpty)
      }
      .render { (props, _, state) =>
        Dialog(
          footer = Button(size = Button.Size.Small,
                          icon = Icons.Close,
                          label = "Close",
                          disabled = !state.value.done,
                          onClick = props.files.set(Nil)
          ),
          closable = false,
          position = DialogPosition.Top,
          visible = props.files.get.nonEmpty,
          clazz = ExploreStyles.Dialog.Small,
          dismissableMask = true,
          resizable = false,
          onHide = props.files.set(Nil),
          header = "Import Targets"
        )(
          <.div(ExploreStyles.TargetImportForm)(
            ProgressSpinner(strokeWidth = "5").unless(state.value.done),
            Icons.Checkmark.withSize(IconSize.X4).when(state.value.done),
            <.div(
              <.span(s"Importing ${state.value.loaded.length}")
                .unless(state.value.done),
              <.span(s"Imported ${state.value.loaded.length} targets").when(state.value.done),
              <.span(s"Import errors: ${state.value.targetErrors}")
            ).when(state.value.genericError.isEmpty),
            <.div(
              <.span(s"Import error: ${state.value.genericError.orEmpty}")
            ).unless(state.value.genericError.isEmpty)
          )
        )
      }
