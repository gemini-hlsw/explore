package explore.targeteditor

import explore.implicits._
import lucuma.core.model.User
import explore.model.TargetEnv
import lucuma.core.model.Target
import explore.undo.UndoStacks
import explore.model.TargetVisualOptions
import cats.effect.IO
import lucuma.core.model.SiderealTarget
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

final case class TargetEnvEditor(
  userId:     Option[User.Id],
  targetEnv:  View[TargetEnv],
  undoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
  searching:  View[Set[Target.Id]],
  options:    View[TargetVisualOptions]
)

object TargetEnvEditor {
  type Props = TargetEnvEditor

  val component =
    ScalaFnComponent[Props](props => <.div)
}
