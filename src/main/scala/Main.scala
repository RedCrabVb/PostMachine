
object Main {

  def main(args: Array[String]): Unit = {
    val argsParser = new ArgsParser(args)
    val input = argsParser.inputLexemes()
    val parser = new Parse(input)
    val ribbon = argsParser.inputRibbon()

    println(input.mkString(", "))
    println(parser.lexemes.mkString(" "))
    println(ribbon.mkString(" "))

    val ribbonObj = new Ribbon(ribbon, argsParser.carriage())



    def callingCommands(numberCommand: Int, out: String = ""): String = {
      val lexeme = parser.lexemes.find(_.number == numberCommand).get

      if (out.split("\n").length/5 > 30) {
        return out + "\nError: Perhaps a cycle"
      }

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
