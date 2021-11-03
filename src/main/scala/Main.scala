import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Main {

  def readInputLexemes(): Array[String] = {
    println("Hello, enter input data (finish enter on 'q')")

    val inputMutable = ListBuffer[String]()
    var i = 1
    do {
      val inputStr = i.toString + " " + readLine(i.toString + " ")
      inputMutable.addOne(inputStr)
      i += 1
    } while ((!inputMutable.last.contains("q")))
    inputMutable.remove(inputMutable.size - 1)

    inputMutable.toArray
  }

  def main(args: Array[String]): Unit = {
    println("hello world")

    val input = readInputLexemes()
    val parser = new Parse(input)

    println(input.mkString(", "))
    println(parser.lexemes.mkString(" "))

    val ribbon = Array.fill(10)(0)
    ribbon(0) = 1
    ribbon(2) = 1
    ribbon(4) = 1
    var index = 2

    def outRibbon(): String = {
      val ribbonList = ribbon.map {
        case 0 => "[ ]"
        case 1 => "[V]"
        case _ => "[error]"
      }
      s"${ribbonList.mkString("")} \n ${" " * 3 * index} _\n"
    }

    def callingCommands(numberCommand: Int, out: String = ""): String = {
      val lexeme = parser.lexemes.find(_.number == numberCommand).get
      def calling(nextCommand: Int) = callingCommands(nextCommand, s"$out $numberCommand $lexeme \n ${outRibbon()}\n")

      lexeme match {
        case MoveBackLexeme(_, nextCommand) => index -= 1; calling(nextCommand)
        case MoveForwardLexeme(_, nextCommand) => index += 1; calling(nextCommand)
        case SetLexeme(_, nextCommand) => ribbon(index) = 1; calling(nextCommand)
        case RemoveLexeme(_, nextCommand) => ribbon(index) = 0; calling(nextCommand)
        case ConditionLexeme(_, numberA, numberB) =>
          val nextCommand = if (ribbon(index) == 0) {
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
