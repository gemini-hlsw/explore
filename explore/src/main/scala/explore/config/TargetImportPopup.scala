// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.*
import cats.effect.*
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.services.OdbApi
import fs2.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.catalog.csv.TargetImport
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.ProgressSpinner
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.Focus
import monocle.Lens
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.syntax.all.*
import org.scalajs.dom.File as DOMFile

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

  private def importTargets[F[+_]: Async](
    programId:   Program.Id,
    s:           Stream[F, Byte],
    stateUpdate: (State => State) => F[Unit],
    client:      Client[F]
  )(odbApi: OdbApi[F]): Stream[F, Unit] =
    s
      .through(text.utf8.decode)
      .through:
        TargetImport.csv2targetsAndLookup(client, uri"https://lucuma-cors-proxy.herokuapp.com".some)
      .evalMap:
        case Left(a)       =>
          stateUpdate(State.targetErrors.modify(e => e :++ a.toList.map(_.displayValue)))
        case Right(target) =>
          odbApi
            .insertTarget(programId, target)
            .flatTap: _ =>
              stateUpdate(l => l.copy(current = target.some, loaded = (target :: l.loaded).reverse))
            .void
      .handleErrorWith: e =>
        Stream.eval(stateUpdate(State.genericError.replace(e.getMessage().some)))

  private val Columns: Reusable[List[ColDef.Type]] = Reusable.always:
    List(
      ColDef(
        ColumnId("Errors"),
        identity,
        size = 1000.toPx,
        enableResizing = false,
        enableSorting = false
      )
    )

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx    <- useContext(AppContext.ctx)
        odbApi <- useContext(OdbApi.ctx)
        state  <- useState(State.Default)
        _      <- useEffectWithDeps(props.files.get): files =>
                    import ctx.given

                    state.setState(State.Default).toAsync *>
                      FetchClientBuilder[IO]
                        .withRequestTimeout(15.seconds)
                        .resource
                        .use: client =>
                          files
                            .traverse: f =>
                              importTargets[IO](
                                props.programId,
                                dom.readReadableStream(IO(f.stream())),
                                state.modState(_).toAsync,
                                client
                              )(odbApi)
                            .compile
                            .toList
                        .guarantee(state.modState(State.done.replace(true)).toAsync)
                        .void
                        .whenA(files.nonEmpty)
        rows   <- useMemo(state.value.targetErrors.length): _ =>
                    state.value.targetErrors
        table  <- useReactTable:
                    TableOptions(
                      Columns,
                      rows,
                      initialState = TableState(
                        columnVisibility = TargetColumns.DefaultVisibility,
                        rowSelection = RowSelection()
                      )
                    )
      yield Dialog(
        footer = Button(
          size = Button.Size.Small,
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
