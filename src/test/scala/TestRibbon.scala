import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TestRibbon extends AnyFlatSpec with should.Matchers {
  val ribbon = new Ribbon(Array.fill(10)(0), 0)

  println(ribbon.outRibbon())


  "Ribbon" should " setup" in {
    ribbon.moveForward()

    ribbon.cellIsEmpty() should be (true)

    ribbon.fill(1)

    ribbon.cellIsEmpty() should be (false)
  }

  it should "move back" in {
    ribbon.cellIsEmpty() should be (false)

    ribbon.moveBack()

    ribbon.cellIsEmpty() should be (true)
  }
}
