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
import explore.model.Constants
import fs2.*
import fs2.text
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.catalog.csv.TargetImport
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.Focus
import monocle.Lens
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.syntax.all.*
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

import scala.concurrent.duration.*

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
    targetErrors: List[String],
    done:         Boolean
  )

  private object State {
    val loaded: Lens[State, List[Target]]         = Focus[State](_.loaded)
    val genericError: Lens[State, Option[String]] = Focus[State](_.genericError)
    val targetErrors: Lens[State, List[String]]   = Focus[State](_.targetErrors)
    val done: Lens[State, Boolean]                = Focus[State](_.done)
    val Default                                   = State(Nil, none, none, Nil, false)
  }

  private given Reusability[DOMFile] = Reusability.by(_.name)
  private val ColDef                 = ColumnDef[String]

  private def importTargets[F[_]: Concurrent: Logger](
    programId:   Program.Id,
    s:           Stream[F, Byte],
    stateUpdate: (State => State) => F[Unit],
    client:      Client[F]
  )(using TransactionalClient[F, ObservationDB]): Stream[F, Unit] =
    s
      .through(text.utf8.decode)
      .through(
        TargetImport.csv2targetsAndLookup(client, uri"https://lucuma-cors-proxy.herokuapp.com".some)
      )
      .evalMap {
        case Left(a)    =>
          stateUpdate(State.targetErrors.modify(e => e :++ a.toList.map(_.displayValue)))
        case Right(tgt) =>
          // FIXME The backend needs a SED
          val target = Target.Sidereal.unnormalizedSED.replace(Constants.DefaultSED.some)(tgt)
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
          FetchClientBuilder[IO]
            .withRequestTimeout(15.seconds)
            .resource
            .use { client =>
              files
                .traverse(f =>
                  importTargets[IO](props.programId,
                                    dom.readReadableStream(IO(f.stream())),
                                    state.modState(_).to[IO],
                                    client
                  )
                )
                .compile
                .toList
            }
            .guarantee(state.modState(State.done.replace(true)).to[IO])
            .void
            .whenA(files.nonEmpty)
      }
      // cols
      .useMemoBy((_, _, _) => ()) { (props, ctx, _) => _ =>
        List(
          ColDef(
            ColumnId("Errors"),
            identity,
            size = 1000.toPx,
            enableResizing = false,
            enableSorting = false
          )
        )
      }
      // rows
      .useMemoBy((_, _, state, _) => state.value.targetErrors.length) { (props, _, state, _) => _ =>
        state.value.targetErrors
      }
      .useReactTableBy((props, ctx, _, cols, rows) =>
        import ctx.given

        TableOptions(
          cols,
          rows,
          initialState = TableState(
            columnVisibility = TargetColumns.DefaultVisibility,
            rowSelection = RowSelection()
          )
        ),
      )
      .render { (props, _, state, _, rows, table) =>
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
          clazz = ExploreStyles.TargetImportDialog,
          dismissableMask = true,
          resizable = false,
          onHide = props.files.set(Nil),
          header = "Import Targets"
        )(
          <.div(ExploreStyles.TargetImportForm)(
            ProgressSpinner(strokeWidth = "5").unless(state.value.done),
            Icons.Checkmark.withSize(IconSize.X4).when(state.value.done),
            <.div(
              ExploreStyles.TargetImportDescription,
              <.span(s"Importing ${state.value.loaded.length}")
                .unless(state.value.done),
              <.span(s"Imported ${state.value.loaded.length} targets").when(state.value.done),
              <.span(s"Import errors: ${state.value.targetErrors.length}")
            ).when(state.value.genericError.isEmpty),
            <.div(
              ExploreStyles.TargetImportDescription,
              <.span(s"Import error: ${state.value.genericError.orEmpty}")
            ).unless(state.value.genericError.isEmpty),
            <.div(
              ExploreStyles.TargetImportErrors |+| ExploreStyles.ExploreBorderTable,
              PrimeAutoHeightVirtualizedTable(
                table,
                estimateSize = _ => 34.toPx,
                tableMod = ExploreStyles.ExploreTable,
                striped = true,
                compact = Compact.Very
              ).withKey(s"errors-table-${state.value.targetErrors.length}")
            ).when(state.value.targetErrors.nonEmpty)
          )
        )
      }
