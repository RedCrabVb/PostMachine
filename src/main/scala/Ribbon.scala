//50 - 0, 49 - -1, 51 - 1
class Ribbon(private val array: Array[Int], private var carriage: Int) {
  carriage += array.size / 2

  def this() = this(Array.fill[Int](100)(0), 1)

  def moveForward(): Ribbon = {
    carriage += 1;
    this
  }

  def moveBack(): Ribbon = {
    carriage -= 1;
    this
  }

  def fill(containing: Int): Ribbon = {
    array(carriage) = containing;
    this
  }

  def cellIsEmpty(): Boolean = carriage == 0

  def outRibbon(): String = {
    val arrayRibbon = array.zip(0 to array.length).slice(carriage - 5, carriage + 5)
    val carriageStable = carriage
    val ribbonList = arrayRibbon.map(i => i match {
      case (0, `carriageStable`) => "[_]"
      case (1, `carriageStable`) => "[Ʌ̌]"
      case (0, _) => "[ ]"
      case (1, _) => "[V]"
      case _ => "[error]"
    })

    s"${arrayRibbon.map(_._2 - ribbonList.length / 2).map(i => f"$i% 4d").mkString("")} \n   ${ribbonList.mkString(" ")} \n\t\n"
  }
}
