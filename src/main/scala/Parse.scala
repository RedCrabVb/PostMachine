import scala.language.implicitConversions

class Parse(commands: Array[String]) {
  val (moveF, moveB, removeVal, setVal, stop, itIs) = (">", "<", "X", "V", "!", "?")
  val lexemes: Array[Lexeme] = for (command <- commands) yield {
    implicit def anyToInt(any: Any): Int = any.toString.replaceAll("\\.", "").toInt

    command.toUpperCase.split(" ").toList match {
      case number :: `stop` :: _ => Stop(number)
      case number :: `moveF` :: nextCommand :: Nil => MoveForwardLexeme(number, nextCommand)
      case number :: `moveB` :: nextCommand :: Nil => MoveBackLexeme(number, nextCommand)
      case number :: `moveF` :: Nil => new MoveForwardLexeme(number)
      case number :: `moveB` :: Nil => new MoveBackLexeme(number)

      case number :: `removeVal` :: nextCommand :: Nil => RemoveLexeme(number, nextCommand)
      case number :: `setVal` :: nextCommand :: Nil => SetLexeme(number, nextCommand)
      case number :: `removeVal` :: Nil => new RemoveLexeme(number)
      case number :: `setVal` :: Nil => new SetLexeme(number)

      case number :: `itIs` :: nextCommandA :: nextCommandB :: _ => ConditionLexeme(number, nextCommandA, nextCommandB)
      case _ => throw new RuntimeException("error parse lexeme, " + command)
    }
  }
}
