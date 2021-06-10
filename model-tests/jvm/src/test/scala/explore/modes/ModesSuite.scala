package explore.modes

import munit.CatsEffectSuite
import cats.effect.Resource
import java.io.File
import cats.effect.IO

class ModesSuite extends CatsEffectSuite {
  val csvPath = IO {
    val f =
      new File(this.getClass().getClassLoader().getResource("instrument_matrix.csv").getFile())

    f.toPath()
  }
  val fixture = ResourceSuiteLocalFixture(
    "modes",
    Resource.make(csvPath.flatMap(ModesMatrix[IO]))(_ => IO.unit)
  )

  override def munitFixtures = List(fixture)

  test("csv loading") {
    IO(fixture()).map { x => x.matrix.foreach(println); x.matrix.length }.assertEquals(101)
  }
}
