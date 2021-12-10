// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SimbadSearch
import explore.common.TargetQueriesGQL
import lucuma.core.enum.CatalogName
import lucuma.core.model
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger

protected sealed trait TargetSource[F[_]] {
  def name: String

  def searches(name: NonEmptyString): List[F[List[Target]]]
}

protected object TargetSource {
  case class Program[F[_]: Async](programId: model.Program.Id)(implicit
    client:                                  TransactionalClient[F, ObservationDB]
  ) extends TargetSource[F] {
    val name: String                                                   =
      s"Program $programId"
    override def searches(name: NonEmptyString): List[F[List[Target]]] =
      List(
        TargetQueriesGQL.TargetNameQuery
          .query()
          .map { data =>
            data.scienceTargetGroup
              .map(_.commonTarget)
              // TODO Remove the filter when the API has a name pattern query
              .filter(_.name.value.toLowerCase.startsWith(name.value.toLowerCase))
              .distinct
          }
      )
  }

  case class Catalog[F[_]: Async: Logger](catalogName: CatalogName) extends TargetSource[F] {
    val name: String                                                   =
      Enumerated[CatalogName].tag(catalogName)
    override def searches(name: NonEmptyString): List[F[List[Target]]] =
      catalogName match {
        case CatalogName.Simbad =>
          val escapedName: String                             = name.value.replaceAll("\\*", "\\\\*")
          val regularSearch: F[List[model.SiderealTarget]]    =
            SimbadSearch.search[F](name)
          // This a heuristic based on observed Simbad behavior.
          val wildcardSearches: List[F[List[SiderealTarget]]] = List(
            NonEmptyString.unsafeFrom(s"$escapedName*"),
            NonEmptyString.unsafeFrom(s"NAME $escapedName*"),
            NonEmptyString.unsafeFrom(
              s"${escapedName.replaceFirst("([A-Za-z-\\.]+)(\\S.*)", "$1 $2")}*"
            )
          ).distinct.map(term =>
            Logger[F].debug(s"Searching Simbad: [$term]") >>
              SimbadSearch.search[F](term, wildcard = true)
          )

          (regularSearch +: wildcardSearches).map((search: F[List[SiderealTarget]]) =>
            search.map((ts: List[SiderealTarget]) => ts: List[Target])
          )
        case _                  => List.empty
      }
  }

  def forAllCatalogs[F[_]: Async: Logger]: List[TargetSource[F]] =
    Enumerated[CatalogName].all.map(source => TargetSource.Catalog(source))

  // TODO Test
  implicit def orderTargetSource[F[_]]: Order[TargetSource[F]] = Order.from {
    case (TargetSource.Program(pidA), TargetSource.Program(pidB)) => pidA.compare(pidB)
    case (TargetSource.Program(_), _)                             => -1
    case (_, TargetSource.Program(_))                             => 1
    case (TargetSource.Catalog(cnA), TargetSource.Catalog(cnB))   => cnA.compare(cnB)
  }
}
