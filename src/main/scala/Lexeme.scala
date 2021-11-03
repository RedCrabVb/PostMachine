sealed trait Lexeme {
  val number: Int
}

case class MoveForwardLexeme(number: Int, nextCommand: Int) extends Lexeme

case class MoveBackLexeme(number: Int, nextCommand: Int) extends Lexeme

case class SetLexeme(number: Int, nextCommand: Int) extends Lexeme

case class RemoveLexeme(number: Int, nextCommand: Int) extends Lexeme

case class ConditionLexeme(number: Int, nextCommandA: Int, nextCommandB: Int) extends Lexeme

case class Stop(number: Int) extends Lexeme