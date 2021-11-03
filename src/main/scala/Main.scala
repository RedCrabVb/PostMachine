

object Main {

  def main(args: Array[String]): Unit = {
    println("hello world")

    val argsParser = new ArgsParser(args)
    val input = argsParser.inputLexemes
    val parser = new Parse(input)
    val ribbon = argsParser.inputRibbon
    var carriage = 2

    println(input.mkString(", "))
    println(parser.lexemes.mkString(" "))

    def outRibbon(): String = {
      val ribbonList = ribbon.map {
        case 0 => "[ ]"
        case 1 => "[V]"
        case _ => "[error]"
      }
      s"${ribbonList.mkString("")} \n ${" " * 3 * carriage} _\n"
    }

    def callingCommands(numberCommand: Int, out: String = ""): String = {
      val lexeme = parser.lexemes.find(_.number == numberCommand).get
      def calling(nextCommand: Int) = callingCommands(nextCommand, s"$out $numberCommand $lexeme \n ${outRibbon()}\n")

      lexeme match {
        case MoveBackLexeme(_, nextCommand) => carriage -= 1; calling(nextCommand)
        case MoveForwardLexeme(_, nextCommand) => carriage += 1; calling(nextCommand)
        case SetLexeme(_, nextCommand) => ribbon(carriage) = 1; calling(nextCommand)
        case RemoveLexeme(_, nextCommand) => ribbon(carriage) = 0; calling(nextCommand)
        case ConditionLexeme(_, numberA, numberB) =>
          val nextCommand = if (ribbon(carriage) == 0) {
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
