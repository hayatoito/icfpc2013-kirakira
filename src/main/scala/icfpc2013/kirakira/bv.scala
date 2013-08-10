package icfpc2013.kirakira

import scala.util.parsing.combinator._
import scala.collection.mutable
import java.net.URL
import sun.net.httpserver.HttpConnection
import java.net.HttpURLConnection
import java.io.DataOutputStream
import java.io.BufferedReader
import java.io.InputStreamReader

object bv {

  case class Func(id: String, e: Expression)
  case class Environment(parent: Environment) {
    val table = mutable.Map.empty[String, Int]
  }

  //  def applyToFunc(parentEnv: Environment, f: Func, x: Int): Int = {
  //    val env = Environment(parentEnv)
  //  }

  sealed trait OpCode1
  case object Not extends OpCode1
  case object Shl1 extends OpCode1
  case object Shr1 extends OpCode1
  case object Shr4 extends OpCode1
  case object Shr16 extends OpCode1

  sealed trait OpCode2
  case object And extends OpCode2
  case object Or extends OpCode2
  case object Xor extends OpCode2
  case object Plus extends OpCode2

  sealed trait Expression

  case class Constant(n: Int) extends Expression // 0, 1, 2
  // case class ID(x: String) extends Expression
  case class If(e0: Expression, e1: Expression, e2: Expression) extends Expression
  case class Op1(op: OpCode1, e: Expression) extends Expression
  case class Op2(op: OpCode2, e1: Expression, e2: Expression) extends Expression

  val ONE = Constant(1)
  val ZERO = Constant(0)

  object BVExpressionParser extends JavaTokenParsers {
    lazy val expr: Parser[Expression] = constant | plus
    lazy val constant: Parser[Expression] =
      wholeNumber ^^ { case num => Constant(num.toInt) }
    lazy val plus: Parser[Expression] =
      "(" ~ "plus" ~ expr ~ expr ~ ")" ^^ { case "(" ~ "plus" ~ e0 ~ e1 ~ ")" => Op2(Plus, e0, e1) }
    def parse(in: String) = parseAll(expr, in)
  }

  def parse(in: String) = BVExpressionParser.parse(in)

  def eval(e: Expression): Int = e match {
    case Constant(n) => n

    case Op1(Not, e) => ~eval(e)
    case Op1(Shl1, e) => eval(e) << 1
    case Op1(Shr1, e) => eval(e) >>> 1
    case Op1(Shr4, e) => eval(e) >>> 4
    case Op1(Shr16, e) => eval(e) >>> 16

    case Op2(And, e1, e2) => eval(e1) & eval(e2)
    case Op2(Or, e1, e2) => eval(e1) | eval(e2)
    case Op2(Xor, e1, e2) => eval(e1) ^ eval(e2)
    case Op2(Plus, e1, e2) => eval(e1) + eval(e2)

    case If(e0, e1, e2) => if (eval(e0) != 0) eval(e1) else eval(e2)
  }

  def stringify(e: Expression): String = e match {
    case Constant(n) => n.toString

    case Op1(Not, e) => "(not " + stringify(e) + ")"
    case Op1(Shl1, e) => "(shl1 " + stringify(e) + ")"
    case Op1(Shr1, e) => "(shr1 " + stringify(e) + ")"
    case Op1(Shr4, e) => "(shr4 " + stringify(e) + ")"
    case Op1(Shr16, e) => "(shr16 " + stringify(e) + ")"

    case Op2(Xor, e1, e2) => "(xor " + stringify(e1) + " " + stringify(e2) + ")"
    case Op2(Plus, e1, e2) => "(plus " + stringify(e1) + " " + stringify(e2) + ")"
    case Op2(And, e1, e2) => "(and " + stringify(e1) + " " + stringify(e2) + ")"
    case Op2(Or, e1, e2) => "(or " + stringify(e1) + " " + stringify(e2) + ")"
    case If(e0, e1, e2) => "(if " + stringify(e0) + " " + stringify(e1) + " " + stringify(e2) + ")"
  }

  def size(e: Expression): Int = e match {
    case Constant(n) => 1
    case Op1(_, e) => 1 + size(e)
    case Op2(_, e1, e2) => 1 + size(e1) + size(e2)
    case If(e0, e1, e2) => 1 + size(e0) + size(e1) + size(e2)
  }

  case class InputOutput(input: Int, output: Int)

  def matchInput(e: Expression, example: InputOutput): Boolean = (eval(e) == example.output)

  //  def solve(size: Int, examples: Seq[InputOutput]): Expression = {
  //    for {
  //      e <- generate(size)
  //    } {
  //      examples.
  //    }
  //
  //  }
}

object bvmain extends App {
  import bv._
  //  println(eval(ZERO))
  //  println(eval(Op2(And, ZERO, ONE)))

  println(eval(Op1(Shr4, Op2(Plus, ONE, ONE))))

  // println(eval(Op2(Plus, ZERO, Op2(Plus, ONE, ONE))))
  // println(Op2(And, ZERO, ONE))
  //  val program = "(plus 1 (plus 0 1))"
  //  println(parse(program).get)
  //  println(stringify(parse(program).get))

}