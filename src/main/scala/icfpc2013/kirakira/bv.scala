package icfpc2013.kirakira

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

object bv {

  case class BVError() extends RuntimeException

  sealed trait Op1Symbol {
    def symbol: String = getClass.getSimpleName.dropRight(1).toLowerCase
  }

  case object Not extends Op1Symbol
  case object Shl1 extends Op1Symbol
  case object Shr1 extends Op1Symbol
  case object Shr4 extends Op1Symbol
  case object Shr16 extends Op1Symbol

  val operators1 = List(Not, Shl1, Shr1, Shr4, Shr16)

  def toOp1(s: String) = operators1.find(_.symbol == s).get

  sealed trait Op2Symbol {
    def symbol: String = getClass.getSimpleName.dropRight(1).toLowerCase
  }

  case object And extends Op2Symbol
  case object Or extends Op2Symbol
  case object Xor extends Op2Symbol
  case object Plus extends Op2Symbol

  val operators2 = List(And, Or, Xor, Plus)

  def toOp2(s: String) = operators2.find(_.symbol == s).get

  sealed trait Expression

  case class Constant(n: Long) extends Expression
  case class Id(x: String) extends Expression
  case class If0(e0: Expression, e1: Expression, e2: Expression) extends Expression
  case class Op1(op: Op1Symbol, e: Expression) extends Expression
  case class Op2(op: Op2Symbol, e1: Expression, e2: Expression) extends Expression
  case class Fold(e0: Expression, e1: Expression, x: String, y: String, e: Expression) extends Expression
  case class Lambda(id: String, e: Expression) extends Expression

  val ZERO = Constant(0)
  val ONE = Constant(1)

  case class Environment(parent: Option[Environment] = None) {
    val table = mutable.Map.empty[String, Long]
    def lookup(id: String): Long = table.get(id) match {
      case Some(value) => value
      case None => parent match {
        case Some(parentEnv) => parentEnv.lookup(id)
        case None => throw BVError()
      }
    }
  }

  def applyFunction(f: Lambda, x: Long): Long = {
    val env = Environment()
    env.table(f.id) = x
    eval(f.e, env)
  }

  def lambda(e: Expression) = Lambda("x", e)

  def evalAsLambda(e: Expression, x: Long): Long = applyFunction(lambda(e), x)

  object Parser extends JavaTokenParsers {
    lazy val expr: Parser[Expression] = constant | id | if0 | op1 | op2 | fold | lambda
    lazy val constant = zero | one
    lazy val zero = "0" ^^ { _ => ZERO }
    lazy val one = "1" ^^ { _ => ONE }
    lazy val id = ident ^^ { case x => Id(x) }
    lazy val op1 = "(" ~ ident ~ expr ~ ")" ^^ { case "(" ~ v ~ e ~ ")" => Op1(toOp1(v), e) }
    lazy val op2 = "(" ~ ident ~ expr ~ expr ~ ")" ^^ { case "(" ~ v ~ e1 ~ e2 ~ ")" => Op2(toOp2(v), e1, e2) }
    lazy val if0 = "(" ~ "if0" ~ expr ~ expr ~ expr ~ ")" ^^ { case "(" ~ "if0" ~ e0 ~ e1 ~ e2 ~ ")" => If0(e0, e1, e2) }
    lazy val fold = "(" ~ "fold" ~ expr ~ expr ~ "(" ~ "lambda" ~ "(" ~ ident ~ ident ~ ")" ~ expr ~ ")" ~ ")" ^^
      { case "(" ~ "fold" ~ e0 ~ e1 ~ "(" ~ "lambda" ~ "(" ~ x ~ y ~ ")" ~ e2 ~ ")" ~ ")" => Fold(e0, e1, x, y, e2) }
    lazy val lambda = "(" ~ "lambda" ~ "(" ~ ident ~ ")" ~ expr ~ ")" ^^
      { case "(" ~ "lambda" ~ "(" ~ x ~ ")" ~ e ~ ")" => Lambda(x, e) }
    def parse(in: String) = parseAll(expr, in)
  }

  def parse(in: String): Expression = Parser.parse(in).get

  def eval(e: Expression, env: Environment = null): Long = e match {
    case Constant(n) => n

    case Op1(Not, e) => ~eval(e, env)
    case Op1(Shl1, e) => eval(e, env) << 1
    case Op1(Shr1, e) => eval(e, env) >>> 1
    case Op1(Shr4, e) => eval(e, env) >>> 4
    case Op1(Shr16, e) => eval(e, env) >>> 16

    case Op2(And, e1, e2) => eval(e1, env) & eval(e2, env)
    case Op2(Or, e1, e2) => eval(e1, env) | eval(e2, env)
    case Op2(Xor, e1, e2) => eval(e1, env) ^ eval(e2, env)
    case Op2(Plus, e1, e2) => eval(e1, env) + eval(e2, env)

    case Id(x) => env.lookup(x)
    case If0(e0, e1, e2) => if (eval(e0, env) == 0) eval(e1, env) else eval(e2, env)
    case Fold(e0, e1, x, y, e) => {
      val n0 = eval(e0, env)
      val n1 = eval(e1, env)
      (0 until 8).toList.foldLeft(n1)((xs, i) => {
        val byte = (n0 >>> (i * 8)) & 0xFF
        val newEnv = Environment(Some(env))
        newEnv.table(x) = byte
        newEnv.table(y) = xs
        eval(e, newEnv)
      })
    }
    case Lambda(x, e) => throw BVError()
  }

  def stringify(e: Expression): String = e match {
    case Constant(n) => n.toString
    case Op1(op1, e) => s"(${op1.symbol} ${stringify(e)})"
    case Op2(op2, e1, e2) => s"(${op2.symbol} ${stringify(e1)} ${stringify(e2)})"
    case Id(x) => x
    case If0(e0, e1, e2) => s"(if0 ${stringify(e0)} ${stringify(e1)} ${stringify(e2)})"
    case Fold(e0, e1, x, y, e) => s"(fold ${stringify(e0)} ${stringify(e1)} (lambda (${x} ${y}) ${stringify(e)}))"
    case Lambda(x, e) => s"(lambda (${x}) ${stringify(e)})"
  }

  def size(e: Expression): Int = e match {
    case Constant(n) => 1
    case Op1(_, e) => 1 + size(e)
    case Op2(_, e1, e2) => 1 + size(e1) + size(e2)
    case Id(_) => 1
    case If0(e0, e1, e2) => 1 + size(e0) + size(e1) + size(e2)
    case Fold(e0, e1, x, y, e) => 2 + size(e0) + size(e1) + size(e)
    case Lambda(x, e) => 1 + size(e)
  }
}
