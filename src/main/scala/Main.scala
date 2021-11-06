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

    s"${arrayRibbon.map(_._2 - 50).map(i => f"$i% 4d").mkString("")} \n   ${ribbonList.mkString(" ")} \n\t\n"
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val argsParser = new ArgsParser(args)
    val input = argsParser.inputLexemes()
    val parser = new Parse(input)
    val ribbon = argsParser.inputRibbon()

    println(input.mkString(", "))
    println(parser.lexemes.mkString(" "))
    println(ribbon.mkString(" "))

    val ribbonObj = new Ribbon()


    def callingCommands(numberCommand: Int, out: String = ""): String = {
      val lexeme = parser.lexemes.find(_.number == numberCommand).get

      def calling(nextCommand: Int) = callingCommands(nextCommand, s"$out $numberCommand $lexeme \n ${ribbonObj.outRibbon()}\n")

      lexeme match {
        case MoveBackLexeme(_, nextCommand) => ribbonObj.moveBack(); calling(nextCommand)
        case MoveForwardLexeme(_, nextCommand) => ribbonObj.moveForward(); calling(nextCommand)
        case SetLexeme(_, nextCommand) => ribbonObj.fill(1); calling(nextCommand)
        case RemoveLexeme(_, nextCommand) => ribbonObj.fill(0); calling(nextCommand)
        case ConditionLexeme(_, numberA, numberB) =>
          val nextCommand = if (ribbonObj.cellIsEmpty) {
            numberA
          } else {
            numberB
          }
          calling(nextCommand)
        case Stop(_) => out
      }
    }

    val out = callingCommands(1)
    println(out)
  }
}
