package explore.targeteditor

import cats.syntax.all._
import explore.implicits._
// import react.common._
// import react.common.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.common.TargetObsQueries._
import reactST.reactTable._
import explore.model.Focused
import crystal.react.implicits._
import react.semanticui.collections.table._
import lucuma.core.model.Target

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  focused:          View[Option[Focused]]
)(implicit val ctx: AppContextIO)
// extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  private val TargetTable = TableMaker[TargetResult]

  private val TargetTableComponent = new SUITable(TargetTable)

  // TODO Move this to a trait in React Common
  implicit def render(props: TargetSummaryTable): VdomElement =
    component(props).vdomElement

  val component =
    ScalaFnComponent[Props] { props =>
      implicit val ctx = props.ctx

      def targetObservations(id: Target.Id): List[ObsResult] =
        props.pointingsWithObs.observations.toList.filter(_.pointing match {
          case Some(PointingTargetResult(tid)) => tid === id
          case _                               => false
        })

      val columns = List(
        TargetTable
          .Column(
            "icon",
            _ => ""
          )
          .setHeader(" "),
        TargetTable
          .Column(
            "name",
            target =>
              <.a(^.onClick ==> (_ =>
                    props.focused.set(Focused.FocusedTarget(target.id).some).runAsyncCB
                  ),
                  target.name.value
              ).rawElement
          )
          .setHeader("Name"),
        TargetTable
          .Column(
            "ra",
            _ => ""
          )
          .setHeader("RA"),
        TargetTable
          .Column(
            "dec",
            _ => ""
          )
          .setHeader("Dec"),
        TargetTable
          .Column(
            "priority",
            _ => ""
          )
          .setHeader("Priority"),
        TargetTable
          .Column(
            "vmag",
            _ => ""
          )
          .setHeader("Vmag"),
        TargetTable
          .Column(
            "count",
            target => targetObservations(target.id).length
          )
          .setHeader("Count"),
        TargetTable
          .Column(
            "observations",
            target =>
              <.span(
                targetObservations(target.id)
                  .map(obs =>
                    <.a(^.onClick ==> (_ =>
                          (props.focused.set(Focused.FocusedObs(obs.id).some) >> cats.effect.IO
                            .println(
                              "EXPAND TARGET!"
                            )).runAsyncCB
                        ),
                        obs.id.toString()
                    )
                  )
                  .mkReactFragment(", ")
              ).rawElement
          )
          .setHeader("Observations")
      ).toJSArray

      TargetTableComponent(
        Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very),
        header = true
      )(TargetTable.Options(columns, props.pointingsWithObs.targets.toList.toJSArray))
    }
  // .builder[Props]
  // .initialState(State())
  // .renderBackend[Backend]
  // .componentDidMount { $ =>
  //   implicit val ctx = $.props.ctx

  //   <.div
  // }
  // .configure(Reusability.shouldComponentUpdate)
  // .build
}
