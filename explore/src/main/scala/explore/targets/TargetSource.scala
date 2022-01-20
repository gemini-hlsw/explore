// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SimbadSearch
import explore.common.TargetQueriesGQL
import lucuma.catalog.CatalogTargetResult
import lucuma.core.enum.CatalogName
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger
import japgolly.scalajs.react.Reusability

protected sealed trait TargetSource[F[_]] {
  def name: String
  def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]]
}

protected object TargetSource {
  case class FromProgram[F[_]: Async](programId: Program.Id)(implicit
    client:                                      TransactionalClient[F, ObservationDB]
  ) extends TargetSource[F] {
    val name: String = s"Program $programId"

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      List(
        TargetQueriesGQL.TargetNameQuery
          .query()
          .map { data =>
            data.targetGroup.nodes
              .map(node => TargetSearchResult(node.target.toOptId, none))
              // TODO Remove the filter when the API has a name pattern query
              .filter(_.target.name.value.toLowerCase.startsWith(name.value.toLowerCase))
              .distinct
          }
      )

    override def toString: String = programId.toString
  }

  case class FromCatalog[F[_]: Async: Logger](catalogName: CatalogName) extends TargetSource[F] {
    val name: String = Enumerated[CatalogName].tag(catalogName)

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      catalogName match {
        case CatalogName.Simbad =>
          val escapedName: String                                  = name.value.replaceAll("\\*", "\\\\*")
          val regularSearch: F[List[CatalogTargetResult]]          =
            SimbadSearch.search[F](name)
          // This a heuristic based on observed Simbad behavior.
          val wildcardSearches: List[F[List[CatalogTargetResult]]] = List(
            NonEmptyString.unsafeFrom(s"$escapedName*"),
            NonEmptyString.unsafeFrom(s"NAME $escapedName*"),
            NonEmptyString.unsafeFrom(
              s"${escapedName.replaceFirst("([A-Za-z-\\.]+)(\\S.*)", "$1 $2")}*"
            )
          ).distinct.map(term =>
            Logger[F].debug(s"Searching Simbad: [$term]") >>
              SimbadSearch.search[F](term, wildcard = true)
          )

          (regularSearch +: wildcardSearches).map((search: F[List[CatalogTargetResult]]) =>
            search.map(
              _.map((r: CatalogTargetResult) => TargetSearchResult.fromCatalogTargetResult(r))
            )
          )
        case _                  => List.empty
      }

    override def toString: String = catalogName.toString
  }

  def forAllCatalogs[F[_]: Async: Logger]: List[TargetSource[F]] =
    Enumerated[CatalogName].all.map(source => TargetSource.FromCatalog(source))

  // TODO Test
  implicit def orderTargetSource[F[_]]: Order[TargetSource[F]] = Order.from {
    case (TargetSource.FromProgram(pidA), TargetSource.FromProgram(pidB)) => pidA.compare(pidB)
    case (TargetSource.FromProgram(_), _)                                 => -1
    case (_, TargetSource.FromProgram(_))                                 => 1
    case (TargetSource.FromCatalog(cnA), TargetSource.FromCatalog(cnB))   => cnA.compare(cnB)
  }

  implicit def reuseTargetSource[F[_]]: Reusability[TargetSource[F]] = Reusability.byEq
}
