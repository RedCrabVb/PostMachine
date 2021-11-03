class Parse(commands: Array[String]) {

  val lexemes = for (command <- commands) yield {
    implicit def anyToInt(any: Any): Int = (any.toString.replaceAll("\\.", "").toInt)

    command.toUpperCase.split(" ").toList match {
      case number :: "!" :: _ => Stop(number)
      case number :: ">" :: nextCommand :: _ => MoveForwardLexeme(number, nextCommand)
      case number :: "<" :: nextCommand :: _ => MoveBackLexeme(number, nextCommand)
      case number :: "X" :: nextCommand :: _ => RemoveLexeme(number, nextCommand)
      case number :: "V" :: nextCommand :: _ => SetLexeme(number, nextCommand)
      case number :: "?" :: nextCommandA :: nextCommandB :: _ => ConditionLexeme(number, nextCommandA, nextCommandB)
      case error => throw new RuntimeException("error parse lexeme, " + command)
    }
  }
}
