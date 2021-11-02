// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Decoder._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.decoders._
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

case class TargetEnvGroup(
  id:             TargetEnvGroupIdSet,
  scienceTargets: TreeSeqMap[TargetIdSet, Target]
) {
  lazy val targetEnvIds: TargetEnvIdSet      = id.targetEnvIds
  lazy val obsIds: SortedSet[Observation.Id] = id.obsIds
  lazy val targetIds: Set[Target.Id]         = this.scienceTargets.keys.toList.map(_.toSortedSet).combineAll

  lazy val name: NonEmptyString =
    if (scienceTargets.isEmpty) NonEmptyString("<No Targets>")
    else NonEmptyString.unsafeFrom(scienceTargets.map(TargetWithId.name.get).mkString(";"))

  // Note: This should only be used while waiting for the server roundtrip
  // since it does not include the target ids. If we start generating
  // target ids on the ui-side, this should go away.
  def addIds(newIds: TargetEnvGroupIdSet): TargetEnvGroup =
    TargetEnvGroup.id.modify(_ ++ newIds)(this)

  def asObsKeyValue: (TargetEnvGroupIdSet, TargetEnvGroup) = (this.id, this)

  def areScienceTargetsEqual(other: TargetEnvGroup): Boolean =
    TargetEnvGroup.areScienceTargetsEqual(this, other)

  /**
   * Effectively creates a subset of the original TargetEnv, where the TargetEnv.id contains only
   * ids in envObsIdsToInclude, and the keys for the TargetEnv.scienceTargets map contain only the
   * keys specified in targetIdsToInclude. This subset can then be passed to the TargetEnvEditor.
   */
  def filterInIds(
    envObsIdsToInclude: TargetEnvGroupIdSet,
    targetIdsToInclude: Set[Target.Id]
  ): Option[TargetEnvGroup] = {
    val filteredIds: Option[TargetEnvGroupIdSet] =
      this.id.filterOpt(envObsIdsToInclude.contains)

    val filteredTargets: Option[TreeSeqMap[TargetIdSet, Target]] =
      this.scienceTargets.toList
        .traverse { case (ids, target) =>
          ids
            .filterOpt(targetIdsToInclude.contains)
            .map((_, target))
        }
        .map(TreeSeqMap.from)
    (filteredIds, filteredTargets).mapN { case (id, targets) => TargetEnvGroup(id, targets) }
  }

  /**
   * Creates a subset of the original TargetEnv where the TargetEnv.id has had all of the ids in
   * envObsIdsToExclude removed, and the TargetEnv.scienceTarget keys have had any ids in
   * targetIdsToExclude removed. This is used to create a new TargetEnv when a subset of the
   * original has been edited, necessitating a split. NOTE: toFilterOut should be a subset of the
   * original with identical targets.
   */
  def filterOutIds(
    envObsIdsToExclude: TargetEnvGroupIdSet,
    targetIdsToExclude: Set[Target.Id]
  ): Option[TargetEnvGroup] = {
    val filteredIds: Option[TargetEnvGroupIdSet]                 =
      this.id.remove(envObsIdsToExclude)
    val filteredTargets: Option[TreeSeqMap[TargetIdSet, Target]] =
      this.scienceTargets.toList
        .map { case (ids, target) =>
          ids.removeSet(targetIdsToExclude).map((_, target))
        }
        .sequence
        .map(TreeSeqMap.from)
    (filteredIds, filteredTargets).mapN { case (id, targets) => TargetEnvGroup(id, targets) }
  }

  /**
   * Given another TargetEnv, this creates a new one where TargetEnv.id is a union of the this.id
   * and other.id. Each of the keys for the targets in TargetEnv.scienceTargets will be the union of
   * the keys in the respective targets in this and other. This is used to merge 2 TargetEnvs in the
   * instance where editing a TargetEnv makes it equal to an existing TargetEnv, making a merger
   * necessary, NOTE: The target lists of this and other are assumed to be equal.
   */
  def merge(other: TargetEnvGroup): TargetEnvGroup = {
    val mergedTargets: TreeSeqMap[TargetIdSet, Target] =
      TreeSeqMap.from(
        (this.scienceTargets.toList, other.scienceTargets.toList)
          .parMapN { case ((ids1, target), (ids2, _)) => (ids1 ++ ids2, target) }
      )
    TargetEnvGroup(this.id ++ other.id, mergedTargets)
  }
}

object TargetEnvGroup {

  /**
   * Compare the targets in the scienceTargets of 2 TargetEnvs to see if they are all equal. This is
   * used to determine if a merger is necessary after an edit.
   */
  def areScienceTargetsEqual(env1: TargetEnvGroup, env2: TargetEnvGroup): Boolean =
    areTargetListsEqual(env1.scienceTargets, env2.scienceTargets)

  def areTargetListsEqual(
    tl1: TreeSeqMap[TargetIdSet, Target],
    tl2: TreeSeqMap[TargetIdSet, Target]
  ): Boolean =
    tl1.size === tl2.size &&
      (tl1.toList, tl2.toList).parMapN { case ((_, t1), (_, t2)) => t1 === t2 }.forall(identity)

  implicit val eqTargetEnv: Eq[TargetEnvGroup] = Eq.by(x => (x.id, x.scienceTargets.toMap))

  private val singleTargetIdDecoder: Decoder[TargetIdSet]       =
    Decoder.instance(_.get[Target.Id]("id").map(id => TargetIdSet.one(id)))
  private val multipleTargetIdDecoder: Decoder[TargetIdSet]     =
    Decoder.instance(
      _.get[List[Target.Id]]("ids").map(list => TargetIdSet.of(list.head, list.tail: _*))
    )
  private implicit val targetIdSetDecoder: Decoder[TargetIdSet] =
    singleTargetIdDecoder.or(multipleTargetIdDecoder)

  private implicit val targetWithIdDecoder: Decoder[TargetWithId] = Decoder.instance(c =>
    for {
      id     <- c.as[TargetIdSet]
      target <- c.as[Target]
    } yield (id, target)
  )

  private val obsIdDecoder: Decoder[Observation.Id] = Decoder.instance(_.get[Observation.Id]("id"))

  private implicit val targetEnvIdDecoder: Decoder[TargetEnvGroupId] = Decoder.instance(c =>
    for {
      targetEnvId <- c.get[TargetEnvironment.Id]("id")
      obsId       <- c.get[Option[Observation.Id]]("observation")(decodeOption(obsIdDecoder))
    } yield TargetEnvGroupId((targetEnvId, obsId))
  )

  private val singleTargetEnvDecoder: Decoder[TargetEnvGroup] = Decoder.instance(c =>
    for {
      id             <- c.as[TargetEnvGroupId].map(id => NonEmptySet.one(id))
      scienceTargets <- c.get[List[TargetWithId]]("scienceTargets").map(TreeSeqMap.from)
    } yield TargetEnvGroup(TargetEnvGroupIdSet(id), scienceTargets)
  )

  private val groupTargetEnvDecoder: Decoder[TargetEnvGroup] = Decoder.instance(c =>
    for {
      id             <- c.get[List[TargetEnvGroupId]]("targetEnvironments")
                          .map(list => NonEmptySet.of(list.head, list.tail: _*))
      scienceTargets <- c.get[List[TargetWithId]]("commonTargetList").map(TreeSeqMap.from)
    } yield TargetEnvGroup(TargetEnvGroupIdSet(id), scienceTargets)
  )

  implicit val decoderTargetEnv: Decoder[TargetEnvGroup] =
    singleTargetEnvDecoder.or(groupTargetEnvDecoder)

  val id: Lens[TargetEnvGroup, TargetEnvGroupIdSet]                         = Focus[TargetEnvGroup](_.id)
  val scienceTargets: Lens[TargetEnvGroup, TreeSeqMap[TargetIdSet, Target]] =
    Focus[TargetEnvGroup](_.scienceTargets)
}
