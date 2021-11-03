import ArgsParser.{MapOptional, help}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.sys.exit

object ArgsParser {
  type MapOptional = Map[String, String]
  private val help =
    """
      |This is program emulator Post Machine
      |====================================================================================
      |--help                           - parameter for displaying this help
      |--inpath [path file]             - path to text file with code [optional, default console input]
      |--inpathr [path file]            - path to text ribbon [optional, default console input]
      |--carriage [number]              - start value of the caret [optional, default console input]
      |Syntax:
      |1. [Command number] x [Command name] 9 [next command, optional]
      |
      |An example program adding two numbers
      |
      |""".stripMargin

}


class ArgsParser(val args: Array[String]) {
  val mapOption: MapOptional = nextOption(args.toList)

  def readInputLexemes(): Array[String] = {
    println("Hello, enter input data (finish enter on 'q')")

    val inputMutable = ListBuffer[String]()
    var i = 1
    do {
      val inputStr = i.toString + " " + readLine(i.toString + " ")
      inputMutable.addOne(inputStr)
      i += 1
    } while (!inputMutable.last.contains("q"))
    inputMutable.remove(inputMutable.size - 1)

    inputMutable.toArray
  }

  val inputLexemes: Array[String] = {
    val inpath: String = mapOption.getOrElse("inpath", null)
    if (inpath != null) {
      Source.fromFile(inpath).getLines().toArray
    } else {
      readInputLexemes()
    }
  }

  val inputRibbon: Array[Int] = {
    val inpath: String = mapOption.getOrElse("inpathr", null)
    if (inpath != null) {
//      new File(inpath).toString.toArray
      null
    } else {
      val ribbon = Array.fill(10)(0)
      ribbon(0) = 1
      ribbon(1) = 1
      ribbon(2) = 1
      ribbon(4) = 1
      ribbon(5) = 1
      ribbon
    }
  }

  @tailrec
  private final def nextOption(list: List[String], map: MapOptional = Map()): MapOptional = {
    list match {
      case Nil => map
      case key :: value :: tail => nextOption(tail, Map(key.substring(2) -> value) ++ map)
      case "--help" :: _ => println(help); exit(0)
      case _ => throw new RuntimeException("Not valid optional")
    }
  }

}
