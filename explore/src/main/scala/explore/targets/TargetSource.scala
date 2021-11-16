// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.effect.Async
import cats.effect.Sync
import cats.kernel.Order
import cats.syntax.all._
import clue.TransactionalClient
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SimbadSearch
import explore.common.TargetQueriesGQL
import lucuma.core.enum.CatalogName
import lucuma.core.model
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger
import cats.Parallel

protected sealed trait TargetSource[F[_]] {
  val name: String

  def search(name: NonEmptyString): F[List[Target]]
}

protected object TargetSource {
  case class Program[F[_]: Async](programId: model.Program.Id)(implicit
    client:                                  TransactionalClient[F, ObservationDB]
  ) extends TargetSource[F] {
    val name: String                                           =
      s"Program $programId"
    override def search(name: NonEmptyString): F[List[Target]] =
      TargetQueriesGQL.TargetNameQuery
        .query()
        .map { data =>
          data.scienceTargetGroup
            .map(_.commonTarget)
            // TODO Remove the filter when the API has a name pattern query
            .filter(_.name.value.toLowerCase.startsWith(name.value.toLowerCase))
            .distinct
        }
  }

  case class Catalog[F[_]: Async: Parallel: Logger](catalogName: CatalogName)
      extends TargetSource[F] {
    val name: String                                           =
      Enumerated[CatalogName].tag(catalogName)
    override def search(name: NonEmptyString): F[List[Target]] =
      catalogName match {
        case CatalogName.Simbad =>
          // This a heuristic based on observed Simbad behavior.
          val terms = List(
            NonEmptyString.unsafeFrom(s"$name*"),
            NonEmptyString.unsafeFrom(s"NAME $name*"),
            NonEmptyString.unsafeFrom(
              s"${name.value.replaceFirst("([A-Za-z-\\.]+)(\\S.*)", "$1 $2")}*"
            )
          ).distinct
          terms
            .parTraverse(term =>
              Logger[F].debug(s"Searching Simbad: [$term]") >>
                SimbadSearch.search[F](term, wildcard = true)
            )
            .map(
              _.flatten
                .distinctBy(t => t.tracking.catalogId.map(_.id).getOrElse(t.name))
                .sortBy(_.name.value)
            )

        case _ => Sync[F].delay(List.empty)
      }
  }

  def forAllCatalogs[F[_]: Async: Parallel: Logger]: List[TargetSource[F]] =
    Enumerated[CatalogName].all.map(source => TargetSource.Catalog(source))

  // TODO Test
  implicit def orderTargetSource[F[_]]: Order[TargetSource[F]] = Order.from {
    case (TargetSource.Program(pidA), TargetSource.Program(pidB)) => pidA.compare(pidB)
    case (TargetSource.Program(_), _)                             => -1
    case (_, TargetSource.Program(_))                             => 1
    case (TargetSource.Catalog(cnA), TargetSource.Catalog(cnB))   => cnA.compare(cnB)
  }
}
