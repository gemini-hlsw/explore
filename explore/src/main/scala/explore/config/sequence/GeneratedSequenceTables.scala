// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

sealed trait GeneratedSequenceTables[S, D] {
  def obsId: Observation.Id
  def config: ExecutionConfig[S, D]
  def snPerClass: Map[ObserveClass, SignalToNoise]
}

case class GmosNorthGeneratedSequenceTables(
  obsId:      Observation.Id,
  config:     ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
  snPerClass: Map[ObserveClass, SignalToNoise]
) extends ReactFnProps(GeneratedSequenceTables.gmosNorthComponent)
    with GeneratedSequenceTables[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]

case class GmosSouthGeneratedSequenceTables(
  obsId:      Observation.Id,
  config:     ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
  snPerClass: Map[ObserveClass, SignalToNoise]
) extends ReactFnProps(GeneratedSequenceTables.gmosSouthComponent)
    with GeneratedSequenceTables[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]

object GeneratedSequenceTables:
  private type Props[S, D] = GeneratedSequenceTables[S, D]

  private def componentBuilder[S, D](
    tableComponent: (List[Atom[D]], Option[SignalToNoise]) => VdomNode
  ) =
    ScalaFnComponent
      .withHooks[Props[S, D]]
      .render(props =>
        <.div(ExploreStyles.SequencesPanel)(
          <.h3("Acquisition"),
          props.config.acquisition
            .map(seq =>
              tableComponent(seq.nextAtom +: seq.possibleFuture,
                             props.snPerClass.get(ObserveClass.Acquisition)
              )
            ),
          <.h3("Science"),
          props.config.science
            .map(seq =>
              tableComponent(seq.nextAtom +: seq.possibleFuture,
                             props.snPerClass.get(ObserveClass.Science)
              )
            )
        )
      )

  protected[sequence] val gmosNorthComponent =
    componentBuilder[StaticConfig.GmosNorth, DynamicConfig.GmosNorth](
      GmosNorthSequenceTable(_, _)
    )

  protected[sequence] val gmosSouthComponent =
    componentBuilder[StaticConfig.GmosSouth, DynamicConfig.GmosSouth](
      GmosSouthSequenceTable(_, _)
    )
