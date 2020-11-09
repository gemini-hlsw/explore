// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.View
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.model.utils._
import explore.target.TargetQueries
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.Label
import react.semanticui.sizes.Small

final case class SearchCallback(
  searchTerm: NonEmptyString,
  onComplete: Option[Target] => Callback,
  onError:    Throwable => Callback
) {
  def run: Callback = Callback.empty
}

final case class TargetBody(
  id:      Target.Id,
  target:  View[TargetResult],
  options: View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val baseCoordinates: Coordinates =
    target.zoom(TargetQueries.baseCoordinates).get
}

object TargetBody {

  type Props = TargetBody
  val AladinRef = AladinCell.component

  implicit val propsReuse = Reusability.derive[Props]

  class Backend {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinRef)

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(_.backend.gotoRaDec(coords))
        .toCallback

    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target = props.target.get

        UndoRegion[TargetResult] { undoCtx =>
          val undoSet =
            UndoSet(props.id, props.target, undoCtx.setter)

          val modify = undoSet[
            (NonEmptyString, SiderealTracking, List[Magnitude])
          ](
            targetPropsL,
            { case (n, t, _ /*ms*/ ) =>
              EditSiderealInput.name.set(n.value.some) >>> TargetQueries.UpdateSiderealTracking(t)
            }
          ) _

          val modifyName = undoSet[NonEmptyString](
            unsafeTargetName,
            n => EditSiderealInput.name.set(n.value.some)
          ) _

          val modifyEpoch = undoSet[Epoch](
            TargetQueries.epoch,
            e => TargetQueries.UpdateSiderealTracking.epoch(e.some)
          ) _

          val modifyProperMotionRA = undoSet[Option[ProperMotion.RA]](
            TargetQueries.pmRALens,
            pmRA =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(pmRA, TargetQueries.pmDecLens.get(target))
              )
          ) _

          val modifyProperMotionDec = undoSet[Option[ProperMotion.Dec]](
            TargetQueries.pmDecLens,
            pmDec =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(TargetQueries.pmRALens.get(target), pmDec)
              )
          ) _

          val modifyParallax = undoSet[Option[Parallax]](
            TargetQueries.pxLens,
            TargetQueries.UpdateSiderealTracking.parallax
          ) _

          val modifyRadialVelocity = undoSet[Option[RadialVelocity]](
            TargetQueries.rvLens,
            TargetQueries.UpdateSiderealTracking.radialVelocity
          ) _

          val searchAndSet: SearchCallback => Callback = s =>
            SimbadSearch
              .search(s.searchTerm)
              .attempt
              .runInCBAndThen {
                case Right(r @ Some(Target(n, Right(st), m))) =>
                  modify((n, st, m.values.toList)).runInCB *>
                    gotoRaDec(st.baseCoordinates) *> s.onComplete(r)
                case Right(Some(r))                           => Callback.log(s"Unknown target type $r") *> s.onComplete(none)
                case Right(None)                              => s.onComplete(none)
                case Left(t)                                  => s.onError(t)
              }

          React.Fragment(
            <.div(
              ExploreStyles.TargetGrid,
              <.div(
                CoordinatesForm(target, searchAndSet, gotoRaDec, modifyName),
                Form(size = Small)(
                  ExploreStyles.Grid,
                  ExploreStyles.Compact,
                  ExploreStyles.TargetPropertiesForm,
                  Label(content = s"Proper Motion",
                        basic = true,
                        clazz = ExploreStyles.FormSectionLabel |+| ExploreStyles.ColumnSpan(2)
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.pmRALens).withOnMod(modifyProperMotionRA),
                    ValidFormatInput.fromFormatOptional(pmRAFormat, "Must be a number"),
                    id = "raPM",
                    label = "µ RA",
                    units = "mas/y"
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.pmDecLens).withOnMod(modifyProperMotionDec),
                    ValidFormatInput.fromFormatOptional(pmDecFormat, "Must be a number"),
                    id = "raDec",
                    label = "µ Dec",
                    units = "mas/y"
                  ),
                  InputWithUnits(
                    props.target.zoom(TargetQueries.epoch).withOnMod(modifyEpoch),
                    ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Must be a number"),
                    id = "epoch",
                    label = "Epoch",
                    units = "years"
                  ),
                  InputWithUnits[cats.effect.IO, Option[Parallax]](
                    props.target.zoom(TargetQueries.pxLens).withOnMod(modifyParallax),
                    ValidFormatInput.fromFormatOptional(pxFormat, "Must be a number"),
                    id = "parallax",
                    label = "Parallax",
                    units = "mas"
                  ),
                  RVInput(props.target.zoom(TargetQueries.rvLens), modifyRadialVelocity)
                ),
                MagnitudeForm(target.magnitudes).when(false),
                UndoButtons(target, undoCtx)
              ),
              AladinRef
                .withRef(aladinRef) {
                  AladinCell(
                    props.target.zoom(TargetQueries.baseCoordinates),
                    props.options
                  )
                },
              CataloguesForm(props.options).when(false)
            ),
            <.div(
              ExploreStyles.TargetSkyplotCell,
              WIP(
                SkyPlotSection(props.baseCoordinates)
              ).when(false)
            )
          )
        }
      }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      gotoRaDec(nextProps.baseCoordinates)
        .when(nextProps.baseCoordinates =!= currentProps.baseCoordinates)
        .void
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .configure(Reusability.shouldComponentUpdate)
      .build

}
