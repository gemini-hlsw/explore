// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SimbadSearch
import explore.services.OdbTargetApi
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.catalog.CatalogTargetResult
import lucuma.core.enums.CatalogName
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.util.Enumerated
import org.typelevel.log4cats.Logger

sealed trait TargetSource[F[_]]:
  def name: String
  def existing: Boolean
  def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]]

object TargetSource:
  case class FromProgram[F[_]](programId: Program.Id)(using
    odbApi: OdbTargetApi[F]
  ) extends TargetSource[F]:
    val name: String = s"Program $programId"

    val existing: Boolean = true

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      List(odbApi.searchTargetsByNamePrefix(programId, name))

    override def toString: String = programId.toString

  case class FromCatalog[F[_]: Async: Logger](catalogName: CatalogName) extends TargetSource[F]:
    val name: String = Enumerated[CatalogName].tag(catalogName).capitalize

    val existing: Boolean = false

    // if rv/px/pm are not set, set them to 0
    private def adjustTracking(t: TargetSearchResult) =
      val p =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.parallax)
          .modify:
            case a @ Some(_) => a
            case _           => Parallax.Zero.some
      val m =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.properMotion)
          .modify:
            case a @ Some(_) => a
            case _           => ProperMotion.Zero.some
      val v =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.radialVelocity)
          .modify:
            case a @ Some(_) => a
            case _           => RadialVelocity.Zero.some
      (p >>> m >>> v)(t)

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      catalogName match {
        case CatalogName.Simbad =>
          val escapedName: String = name.value.replaceAll("\\*", "\\\\*")

          val regularSearch: F[List[CatalogTargetResult]] =
            SimbadSearch.search[F](name)

          // This a heuristic based on observed Simbad behavior.
          val wildcardSearches: List[F[List[CatalogTargetResult]]] = List(
            NonEmptyString.unsafeFrom(s"$escapedName*"),
            NonEmptyString.unsafeFrom(
              s"${escapedName.replaceFirst("([A-Za-z-\\.]+)(\\S.*)", "$1 $2")}*"
            ),
            NonEmptyString.unsafeFrom(s"NAME $escapedName*")
          ).distinct.map(term =>
            Logger[F].debug(s"Searching Simbad: [$term]") >>
              SimbadSearch.search[F](term, wildcard = true)
          )

          (regularSearch +: wildcardSearches).map:
            _.map:
              _.map: r =>
                TargetSearchResult.fromCatalogTargetResult(r)
              .map(adjustTracking)
        case _                  => List.empty
      }

    override def toString: String = catalogName.toString

  def forAllCatalogs[F[_]: Async: Logger]: NonEmptyList[TargetSource[F]] =
    NonEmptyList.fromListUnsafe(
      Enumerated[CatalogName].all.map(source => TargetSource.FromCatalog(source))
    )

  def forAllSiderealCatalogs[F[_]: Async: Logger]: NonEmptyList[TargetSource[F]] =
    NonEmptyList.of(TargetSource.FromCatalog(CatalogName.Simbad))

  // TODO Test
  given orderTargetSource[F[_]]: Order[TargetSource[F]] = Order.from {
    case (TargetSource.FromProgram(pidA), TargetSource.FromProgram(pidB)) => pidA.compare(pidB)
    case (TargetSource.FromProgram(_), _)                                 => -1
    case (_, TargetSource.FromProgram(_))                                 => 1
    case (TargetSource.FromCatalog(cnA), TargetSource.FromCatalog(cnB))   => cnA.compare(cnB)
  }

  given reuseTargetSource[F[_]]: Reusability[TargetSource[F]] = Reusability.byEq
