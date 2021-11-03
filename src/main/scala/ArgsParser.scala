import ArgsParser.{MapOptional, help}

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
      |Subtracting two numbers:
      |1 <
      |2 ? 1 3
      |3 x
      |4 >
      |5 ? 4 6
      |6 x
      |7 > 8
      |8 ? 9 1
      |9 !
      |
      |Ribbon:
      |V_6 0_2 v_3
      |""".stripMargin

}


class ArgsParser(val args: Array[String]) {
  val mapOption: MapOptional = nextOption(args.toList)

  def readInputLexemes(): Array[String] = {
    println("Enter input data (finish enter on 'q')")

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

  def readRibbon(ribbonStr: String): Array[Int] = {
    val ribbonArr = ribbonStr
      .toUpperCase
      .replace("V", "1")
      .replace("X", "0")
      .split("-").toList.map(_.split("_"))
      .map(t => (t(0).toInt, t(1).toInt))
    val arrayBuffer = ArrayBuffer[Int]()
    arrayBuffer.addAll(Array(0, 0, 0, 0, 0, 0))
    ribbonArr.map(a => (1 to a._2).map(v => arrayBuffer append a._1))
    while (arrayBuffer.size < 20) {
      arrayBuffer.append(0)
    }
    arrayBuffer.toArray
  }

  val inputLexemes: Array[String] = {
    val inpath: String = mapOption.getOrElse("inpath", null)
    if (inpath != null) {
      Source.fromFile(inpath).getLines().toArray
    } else {
      readInputLexemes()
    }
  }

  val carriage: Int = mapOption.getOrElse("carriage", "1").toInt

  val inputRibbon: Array[Int] = {
    val inpathr: String = mapOption.getOrElse("inpathr", null)
    if (inpathr != null) {
      println(inpathr)
      println(readRibbon(inpathr).mkString(" "))
      readRibbon(inpathr)
    } else {
      val ribbonStr = readLine("Enter ribbon: ")
      readRibbon(ribbonStr)
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
