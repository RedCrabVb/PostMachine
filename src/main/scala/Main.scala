import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world")

    /*    val input =
          """1. ? 2 4
            |2. V 3
            |3. !
            |4. x 5
            |5. > 6
            |6. ? 8 7
            |7. < 6
            |8. > 1
            |""".stripMargin.split("\n")*/
    println("Hello, enter input data (finish enter on 'q')")
    val inputMutable = ListBuffer[String]()
    var inputStr: String = 1.toString + " " + readLine(1.toString + " ")
    var i = 1
    while (!inputStr.contains("q")) {
      i += 1
      inputMutable.addOne(inputStr)
      inputStr = i.toString + " " + readLine(i.toString + " ")
    }

    val input = inputMutable.toArray
    println(input.mkString(", "))

    val parser = new Parse(input)

    println(parser.lexemes.mkString(" "))

    val ribbon = Array.fill(10)(0)
    ribbon(0) = 1
    ribbon(2) = 1
    ribbon(4) = 1
    var index = 2

    def printRibbon(): Unit = {
      val ribbonStr = for (element <- ribbon) yield {
        element match {
          case 0 => "[ ]"
          case 1 => "[V]"
          case _ => "[error]"
        }
      }
      println(ribbonStr.mkString("") + "")
      println(s" ${" " * 3 * index} _\n")
    }

    def callingСommands(numberCommand: Int): Unit = {
      println(numberCommand + " " + parser.lexemes.find(_.number == numberCommand).get)
      val callingNext = parser.lexemes.find(_.number == numberCommand).get match {
        case MoveBackLexeme(_, nextCommand) =>
          index -= 1
          nextCommand
        case MoveForwardLexeme(_, nextCommand) =>
          index += 1
          nextCommand
        case SetLexeme(_, nextCommand) =>
          ribbon(index) = 1
          nextCommand
        case RemoveLexeme(_, nextCommand) =>
          ribbon(index) = 0
          nextCommand
        case ConditionLexeme(_, numberA, numberB) =>
          if (ribbon(index) == 0) {
            numberA
          } else {
            numberB
          }
        case Stop(_) => -1
      }
      printRibbon()
      if (callingNext != -1) {
        callingСommands(callingNext)
      }
    }

    callingСommands(1)

  }
}
