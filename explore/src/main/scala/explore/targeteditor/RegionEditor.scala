// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.*
import crystal.react.hooks.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.display.given
import explore.model.extensions.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ArcType
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.math.validation.MathValidators
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given

final case class RegionEditor(region: View[Region], readonly: Boolean)
    extends ReactFnProps(RegionEditor)

object RegionEditor
    extends ReactFnComponent[RegionEditor](props =>
      extension (arcType: ArcType)
        def toRaArc: Arc[RightAscension] = arcType match
          case ArcType.Empty   => Arc.Empty()
          case ArcType.Full    => Arc.Full()
          case ArcType.Partial =>
            Arc.Partial(RightAscension.Zero, RightAscension(HourAngle.HourAngle12))
        def toDecArc: Arc[Declination]   = arcType match
          case ArcType.Empty   => Arc.Empty()
          case ArcType.Full    => Arc.Full()
          case ArcType.Partial => Arc.Partial(Declination.Min, Declination.Max)

      for
        raType  <- useStateView(props.region.get.raArc.toArcType)
        decType <- useStateView(props.region.get.decArc.toArcType)
        _       <- useEffectWithDeps(props.region.get.raArc): arc =>
                     raType.set(arc.toArcType)
        _       <- useEffectWithDeps(props.region.get.decArc): arc =>
                     decType.set(arc.toArcType)
      yield
        val raArcView   = props.region.zoom(Region.raArc)
        val raStartView = raArcView.zoom(Arc.start[RightAscension])
        val raEndView   = raArcView.zoom(Arc.end[RightAscension])

        val decArcView   = props.region.zoom(Region.decArc)
        val decStartView = decArcView.zoom(Arc.start[Declination])
        val decEndView   = decArcView.zoom(Arc.end[Declination])

        React.Fragment(
          FormEnumDropdownView(
            id = "ra-arc-type".refined,
            value = raType.withOnMod(at => raArcView.set(at.toRaArc)),
            label = React.Fragment("RA Arc", HelpIcon("target/main/region.md".refined)),
            disabled = props.readonly
          ),
          raStartView.mapValue(v =>
            FormInputTextView(
              id = "ra-arc-start".refined,
              value = v,
              label = "start",
              labelClass = ExploreStyles.IndentLabel,
              validFormat = MathValidators.truncatedRA,
              changeAuditor = ChangeAuditor.truncatedRA,
              disabled = props.readonly
            )
          ),
          raEndView.mapValue(v =>
            FormInputTextView(
              id = "ra-arc-end".refined,
              value = v,
              label = "end",
              labelClass = ExploreStyles.IndentLabel,
              validFormat = MathValidators.truncatedRA,
              changeAuditor = ChangeAuditor.truncatedRA,
              disabled = props.readonly
            )
          ),
          FormEnumDropdownView(
            id = "dec-arc-type".refined,
            value = decType.withOnMod(at => decArcView.set(at.toDecArc)),
            label = React.Fragment("Dec Arc", HelpIcon("target/main/region.md".refined)),
            disabled = props.readonly
          ),
          decStartView.mapValue(v =>
            FormInputTextView(
              id = "dec-arc-start".refined,
              value = v,
              label = "start",
              labelClass = ExploreStyles.IndentLabel,
              validFormat = MathValidators.truncatedDec,
              changeAuditor = ChangeAuditor.truncatedDec,
              disabled = props.readonly
            )
          ),
          decEndView.mapValue(v =>
            FormInputTextView(
              id = "dec-arc-end".refined,
              value = v,
              label = "end",
              labelClass = ExploreStyles.IndentLabel,
              validFormat = MathValidators.truncatedDec,
              changeAuditor = ChangeAuditor.truncatedDec,
              disabled = props.readonly
            )
          )
        )
    )
