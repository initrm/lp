package arithmetic_operations

import scala.util.parsing.combinator._

/** Parser for elementary arithmetic expressions */
class ArithmeticOperationsParser extends JavaTokenParsers {

  /** returns Parser[Boolean] */
  def expression = operations ~ result ^^ { case real ~ expected => real == expected }

  /** returns Parser[Integer] */
  def operations = decimalNumber ~ rep1(operation) <~ "=" ^^ {
    case int ~ intList => var acc = Integer.parseInt(int); for (L <- intList) acc += L; acc
  }

  /** returns Parser[Integer] */
  def operation = plus | minus 
  def plus = "+" ~> decimalNumber ^^ { case int => Integer.parseInt(int) }
  def minus = "-" ~> decimalNumber ^^ { case int => Integer.parseInt(int) * -1 }

  /** returns Parser[Integer] */
  def result = rep1("-") ~> decimalNumber ^^ { case int => Integer.parseInt(int)}

}