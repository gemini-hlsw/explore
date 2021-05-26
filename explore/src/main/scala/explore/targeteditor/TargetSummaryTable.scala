// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import explore.common.TargetObsQueries
import explore.common.TargetObsQueries._
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import react.common._
import react.semanticui.collections.table._
import reactST.reactTable._

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds]
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  private val TargetTable = TableMaker[TargetResult]

  private val TargetTableComponent = new SUITable(TargetTable)

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive
  protected class Backend {

    def render(props: Props) = {
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
            (TargetObsQueries.baseCoordinatesRa.get _)
              .andThen(TruncatedRA.rightAscension.get)
              .andThen(ValidFormatInput.truncatedRA.reverseGet)
          )
          .setHeader("RA"),
        TargetTable
          .Column(
            "dec",
            (TargetObsQueries.baseCoordinatesDec.get _)
              .andThen(TruncatedDec.declination.get)
              .andThen(ValidFormatInput.truncatedDec.reverseGet)
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
            _.magnitudes.collectFirst {
              case Magnitude(value, band, _, _) if band === MagnitudeBand.V =>
                MagnitudeValue.fromString.reverseGet(value)
            }.orEmpty
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
                    <.a(
                      ^.onClick ==> (_ =>
                        (props.focused.set(Focused.FocusedObs(obs.id).some) >> props.expandedIds
                          .mod(ExpandedIds.targetIds.modify(_ + target.id))).runAsyncCB
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
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
