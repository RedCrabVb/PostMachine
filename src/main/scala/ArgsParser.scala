import org.apache.commons.cli._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.io.StdIn.readLine
import scala.sys.exit

class ArgsParser(val args: Array[String]) {
  validation()

  def inputLexemes(): Array[String] = _inputLexemes
  def carriage(): Int = _carriage
  def inputRibbon(): Array[Int] = _inputRibbon

  private var _inputLexemes: Array[String] = _
  private var _carriage: Int = _
  private var _inputRibbon: Array[Int] = _


  private[this] def validation(): Unit = {
    val options = new Options()


    val help = new Option("h", "help", false, "parameter for displaying this help")
    val carriage = new Option("c", "carriage", true, "start value of the caret")
    val inpath = new Option("inp", "inpath", true, "path to text file with code")
    val inpathr = new Option("inr", "inpathr", true, "path to text ribbon ")

    options.addOption(help)
      .addOption(help)
      .addOption(carriage)
      .addOption(inpath)
      .addOption(inpathr)

    val parser = new DefaultParser()

    def helpPrint() = {
      val formatter = new HelpFormatter()
      println("This is program emulator Post Machine")
      println("==========================================================================================")
      formatter.printHelp("Lab-runner", options)
      println(""" |Syntax:
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
                |.stripMargin""".stripMargin)
      exit(0)
    }

    try {
      parser.parse(options, args)
      val cmd = parser.parse(options, args)

      if (cmd.hasOption(help)) {
        helpPrint()
      }

      _inputLexemes = if (cmd.hasOption(inpath)) {
        val inpathVar = cmd.getOptionValue(inpath)
        Source.fromFile(inpathVar).getLines().toArray
      } else {
        readInputLexemes()
      }

      _carriage = if (cmd.hasOption(carriage)) {
        cmd.getOptionValue(carriage).toInt
      } else {
        1
      }

      _inputRibbon = if (cmd.hasOption(inpathr)) {
        readRibbon(Source.fromFile(cmd.getOptionValue(inpathr)).mkString(""))
      } else {
        Array.fill(10)(0)
      }
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        helpPrint()
    }
  }

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
    val arrayBuffer = ArrayBuffer.fill(25)(0)
    ribbonArr.map(a => (1 to a._2).map(_ => arrayBuffer append a._1))
    while (arrayBuffer.size < 50) {
      arrayBuffer.append(0)
    }
    arrayBuffer.toArray
  }
}
