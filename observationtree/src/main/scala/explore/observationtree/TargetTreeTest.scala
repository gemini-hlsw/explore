package explore.observationtree

import cats.implicits._
import explore.model.ExploreObservation
import explore.model.SiderealTarget
import gsp.math.RightAscension
import gsp.math.Declination
import gsp.math.ProperMotion
import gsp.math.Coordinates
import gsp.math.Epoch
import java.util.UUID
import explore.model.enum.ObsStatus
import java.time.Duration

object TargetTreeTest {

  def target(name: String, raStr: String, decStr: String): SiderealTarget = {
    val ra     = RightAscension.fromStringHMS.getOption(raStr)
    val dec    = Declination.fromStringSignedDMS.getOption(decStr)
    val coords =
      ProperMotion((ra, dec).mapN(Coordinates.apply).getOrElse(Coordinates.Zero),
                   Epoch.J2000,
                   none,
                   none,
                   none
      )
    SiderealTarget(
      UUID.randomUUID,
      name,
      coords
    )
  }

  val ngc1055 = target("NGC 1055", "02:41:45.232999", "+00:26:35.450016")
  val ngc7752 = target("NGC 7752", "23:46:58.557000", "+29:27:32.169995")
  val ngc3705 = target("NGC 3705", "11:30:07.456000", "+09:16:35.870015")
  val ngc1068 = target("NGC 1068", "02:42:40.771000", "-00:00:47.840004")
  val ngc1087 = target("NGC 1087", "02:46:25.154457", "-00:29:55.449960")

  val targets = List(
    ngc1055,
    ngc7752,
    ngc3705,
    ngc1068,
    ngc1087
  )

  def observation(
    target:      SiderealTarget,
    conf:        String,
    constraints: String,
    duration:    Duration
  ): ExploreObservation =
    ExploreObservation(
      UUID.randomUUID,
      target,
      ObsStatus.New,
      conf,
      constraints,
      duration
    )

  val observations: List[ExploreObservation] =
    List(
      observation(ngc1055, "GMOS-N R831 1x300\"", "<0.8\" <0.3 mag Gray", Duration.ofHours(2)),
      observation(ngc7752, "GMOS-N R831 1x300\"", "<0.8\" <0.3 mag Gray", Duration.ofMinutes(82)),
      observation(ngc1068, "GMOS-N R831 1x300\"", "<0.8\" <0.3 mag Gray", Duration.ofMinutes(105)),
      observation(ngc1068, "GNIRS SXD 0.60\"", "<0.7\" <0.3 mag Bright", Duration.ofMinutes(92))
    )
}
