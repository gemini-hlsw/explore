// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import crystal.react.View
import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*
import clue.TransactionalClient
import crystal.react.reuse.*
import explore.Icons
import crystal.react.implicits.*
import fs2.*
import fs2.text
import lucuma.core.model.Program
import lucuma.core.model.Target
import explore.model.AppContext
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Dialog
import react.primereact.DialogPosition
import react.primereact.ProgressSpinner
import org.scalajs.dom.{File => DOMFile}
import lucuma.schemas.ObservationDB
import queries.common.TargetQueriesGQL.*
import queries.schemas.odb.ODBConversions.*
import lucuma.catalog.csv.TargetImport
import org.typelevel.log4cats.Logger

case class TargetImportPopup(
  programId: Program.Id,
  files:     View[List[DOMFile]]
) extends ReactFnProps(TargetImportPopup.component)

object TargetImportPopup:
  private type Props = TargetImportPopup

  given Reusability[DOMFile] = Reusability.by(_.name)

  def importTargets[F[_]: Spawn: Logger](
    programId:     Program.Id,
    s:             Stream[F, Byte],
    currentTarget: Option[Target] => F[Unit]
  )(using TransactionalClient[F, ObservationDB]) =
    s
      .through(text.utf8.decode)
      .through(TargetImport.csv2targets)
      .evalMap {
        case Left(a)       => Applicative[F].unit
        case Right(target) =>
          Logger[F].info(s"create $target") *>
            currentTarget(target.some).start *>
            CreateTargetMutation
              .execute(target.toCreateTargetInput(programId))
              .map(_.createTarget.target.id)
              .void
      }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(none[Target])
      .useEffectWithDepsBy((props, _, _) => props.files.get) {
        (props, ctx, currentTarget) => files =>
          import ctx.given

          files
            .traverse(f =>
              importTargets[IO](props.programId,
                                dom.readReadableStream(IO(f.stream())),
                                currentTarget.setState(_).to[IO]
              ).compile.toList
            )
            .void
      }
      .render { (props, _, currentTarget) =>
        Dialog(
          footer = Button(size = Button.Size.Small,
                          icon = Icons.Close,
                          label = "Close",
                          disabled = props.files.get.isEmpty,
                          onClick = props.files.set(Nil)
          ),
          //   .withMods(^.key := "input-cancel"),
          position = DialogPosition.Top,
          visible = props.files.get.nonEmpty,
          clazz = ExploreStyles.Dialog.Small,
          dismissableMask = true,
          resizable = false,
          onHide = props.files.set(Nil),
          header = "Import Targets"
          //   <.div(s"${props.obsId}: ${props.title}"),
          //   props.subtitle.map(subtitle => <.div(ExploreStyles.SequenceObsSutitle, subtitle))
          // )
        )(
          <.div(ExploreStyles.TargetImportForm)(
            ProgressSpinner(),
            s"Importing ${currentTarget.value.foldMap(_.name.value)}"
            // ProgressBar(value = 10)
          )
        )
      }
