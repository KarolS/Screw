package karolmstasiak.screw
import scala.util.parsing.combinator._
import scala.io.Source

object Parser extends RegexParsers{
  override val whiteSpace = """[^\.,<>\+\-\[\]]+""".r

  def op:Parser[Expr] = (
    "+" ^^^ IncreaseRelative(0,1) |
    "-" ^^^ IncreaseRelative(0,-1) |
    ">" ^^^ Shift(1) |
    "<" ^^^ Shift(-1) |
    "." ^^^ Write(1) |
    "," ^^^ Read(1) |
    "["~>rep(op)<~"]" ^^(xs=>While(0, xs))
    )
  def parseBf(program:String) = parseAll(rep(op), program).get
}