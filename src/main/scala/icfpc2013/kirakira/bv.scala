package icfpc2013.kirakira

import scala.util.parsing.combinator._
import scala.collection.mutable

object bv {

  // Todo: Implements lambda.
  case class Program(parameterName: String, e: Expression)
  case class Environment(parent: Environment) {
    val table = mutable.Map.empty[String, Int]
  }

  sealed trait Op1Symbol
  case object Not extends Op1Symbol
  case object Shl1 extends Op1Symbol
  case object Shr1 extends Op1Symbol
  case object Shr4 extends Op1Symbol
  case object Shr16 extends Op1Symbol

  def operations1 = List(Not, Shl1, Shr1, Shr4, Shr16)

  sealed trait Op2Symbol
  case object And extends Op2Symbol
  case object Or extends Op2Symbol
  case object Xor extends Op2Symbol
  case object Plus extends Op2Symbol

  def operations2 = List(And, Or, Xor, Plus)

  sealed trait Expression

  case class Constant(n: Int) extends Expression // 0, 1, 2
  // case class ID(x: String) extends Expression
  case class If(e0: Expression, e1: Expression, e2: Expression) extends Expression
  case class Op1(op: Op1Symbol, e: Expression) extends Expression
  case class Op2(op: Op2Symbol, e1: Expression, e2: Expression) extends Expression

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

    case Op1(Not, e) => s"(not ${stringify(e)})"
    case Op1(Shl1, e) => s"(shl1 ${stringify(e)})"
    case Op1(Shr1, e) => s"(shr1 ${stringify(e)})"
    case Op1(Shr4, e) => s"(shr4 ${stringify(e)})"
    case Op1(Shr16, e) => s"(shr16 ${stringify(e)})"

    case Op2(Xor, e1, e2) => s"(xor ${stringify(e1)} ${stringify(e2)})"
    case Op2(Plus, e1, e2) => s"(plus ${stringify(e1)} ${stringify(e2)})"
    case Op2(And, e1, e2) => s"(and ${stringify(e1)} ${stringify(e2)})"
    case Op2(Or, e1, e2) => s"(or ${stringify(e1)} ${stringify(e2)})"
    case If(e0, e1, e2) => s"(if ${stringify(e0)} ${stringify(e1)} ${stringify(e2)})"
  }

  def size(e: Expression): Int = e match {
    case Constant(n) => 1
    case Op1(_, e) => 1 + size(e)
    case Op2(_, e1, e2) => 1 + size(e1) + size(e2)
    case If(e0, e1, e2) => 1 + size(e0) + size(e1) + size(e2)
  }

  case class InputOutput(input: Int, output: Int)

  def constants = List(ZERO, ONE)

  def matchInput(e: Expression, example: InputOutput): Boolean = (eval(e) == example.output)

  def generateOp1(op1Symbol: Op1Symbol): Seq[Expression] = Nil

  case class Operations(op1s: List[Op1Symbol], op2s: List[Op2Symbol])
  
  val allOperations = Operations(operations1, operations2)

  def generateExpressions(size: Int, operations: Operations): Seq[Expression] = {
    if (size == 1) constants
    else {
      (for {
        e1Size <- 1 to size - 1
        op1 <- operations.op1s
        e1 <- generateExpressions(e1Size, operations)
      } yield Op1(op1, e1)) ++
        (for {
          e1Size <- 1 to size - 2
          e2Size = size - 1 - e1Size
          op2 <- operations.op2s
          e1 <- generateExpressions(e1Size, operations)
          e2 <- generateExpressions(e2Size, operations)
        } yield Op2(op2, e1, e2))
    }
  }
}

object bvmain extends App {
  import bv._
  //  println(eval(ZERO))
  //  println(eval(Op2(And, ZERO, ONE)))

  println(eval(Op1(Shr4, Op2(Plus, ONE, ONE))))

  generateExpressions(5, allOperations) map stringify foreach println

  // println(eval(Op2(Plus, ZERO, Op2(Plus, ONE, ONE))))
  // println(Op2(And, ZERO, ONE))
  //  val program = "(plus 1 (plus 0 1))"
  //  println(parse(program).get)
  //  println(stringify(parse(program).get))

}