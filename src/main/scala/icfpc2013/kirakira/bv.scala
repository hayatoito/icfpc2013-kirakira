package icfpc2013.kirakira

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

object bv {

  implicit class RichLong(val n: Long) extends AnyVal {
    def toHex: String = f"0x${n}%08x"
  }

  implicit class RichString(val s: String) extends AnyVal {
    def asLong: Long = {
      assert(s.take(2) == "0x")
      java.lang.Long.parseLong(s.drop(2), 16)
    }
  }

  case class BVError() extends RuntimeException

  sealed trait Op1Symbol {
    def value: String
  }
  case object Not extends Op1Symbol {
    override def value = "not"
  }
  case object Shl1 extends Op1Symbol {
    override def value = "shl1"
  }
  case object Shr1 extends Op1Symbol {
    override def value = "shr1"
  }
  case object Shr4 extends Op1Symbol {
    override def value = "shr4"
  }
  case object Shr16 extends Op1Symbol {
    override def value = "shr16"
  }

  def operators1 = List(Not, Shl1, Shr1, Shr4, Shr16)

  def toOp1(s: String) = operators1.find(_.value == s).get

  sealed trait Op2Symbol {
    def value: String
  }
  case object And extends Op2Symbol {
    def value = "and"
  }
  case object Or extends Op2Symbol {
    def value = "or"
  }
  case object Xor extends Op2Symbol {
    def value = "xor"
  }
  case object Plus extends Op2Symbol {
    def value = "plus"
  }

  def operators2 = List(And, Or, Xor, Plus)

  def toOp2(s: String) = operators2.find(_.value == s).get

  sealed trait Expression

  case class Constant(n: Long) extends Expression
  case class Id(x: String) extends Expression
  case class If(e0: Expression, e1: Expression, e2: Expression) extends Expression
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

  object Parser extends JavaTokenParsers {
    lazy val expr: Parser[Expression] = constant | id | if0 | op1 | op2 | fold | lambda
    lazy val constant = wholeNumber ^^ { case num => Constant(num.toLong) }
    lazy val id = ident ^^ { case x => Id(x) }
    lazy val op1 = "(" ~ ident ~ expr ~ ")" ^^ { case "(" ~ v ~ e ~ ")" => Op1(toOp1(v), e) }
    lazy val op2 = "(" ~ ident ~ expr ~ expr ~ ")" ^^ { case "(" ~ v ~ e1 ~ e2 ~ ")" => Op2(toOp2(v), e1, e2) }
    lazy val if0 = "(" ~ "if" ~ expr ~ expr ~ expr ^^ { case "(" ~ "if" ~ e0 ~ e1 ~ e2 => If(e0, e1, e2) }
    lazy val fold = "(" ~ "fold" ~ expr ~ expr ~ "(" ~ "lambda" ~ "(" ~ ident ~ ident ~ ")" ~ expr ~ ")" ~ ")" ^^
      { case "(" ~ "fold" ~ e0 ~ e1 ~ "(" ~ "lambda" ~ "(" ~ x ~ y ~ ")" ~ e2 ~ ")" ~ ")" => Fold(e0, e1, x, y, e2) }
    lazy val lambda = "(" ~ "lambda" ~ "(" ~ ident ~ ")" ~ expr ~ ")" ^^
      { case "(" ~ "lambda" ~ "(" ~ x ~ ")" ~ e ~ ")" => Lambda(x, e) }
    def parse(in: String) = parseAll(expr, in)
  }

  def parseOption(in: String) = Parser.parse(in)
  def parse(in: String): Expression = {
    val result = parseOption(in)
    if (result.isEmpty) {
      println("failed: " + result)
      throw BVError()
    } else
      result.get
  }

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
    case If(e0, e1, e2) => if (eval(e0, env) != 0) eval(e1, env) else eval(e2, env)
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
    case Op1(op1, e) => s"(${op1.value} ${stringify(e)})"
    case Op2(op2, e1, e2) => s"(${op2.value} ${stringify(e1)} ${stringify(e2)})"
    case Id(x) => x
    case If(e0, e1, e2) => s"(if ${stringify(e0)} ${stringify(e1)} ${stringify(e2)})"
    case Fold(e0, e1, x, y, e) => s"(fold ${stringify(e0)} ${stringify(e1)} (lambda (${x} ${y}) ${stringify(e)}))"
    case Lambda(x, e) => s"(lambda (${x}) ${stringify(e)})"
  }

  def size(e: Expression): Int = e match {
    case Constant(n) => 1
    case Op1(_, e) => 1 + size(e)
    case Op2(_, e1, e2) => 1 + size(e1) + size(e2)
    case Id(_) => 1
    case If(e0, e1, e2) => 1 + size(e0) + size(e1) + size(e2)
    case Fold(e0, e1, x, y, e) => 2 + size(e0) + size(e1) + size(e)
    case Lambda(x, e) => 1 + size(e)
  }

  def constants = List(ZERO, ONE)
  case class Operators(op1s: List[Op1Symbol], op2s: List[Op2Symbol])

  val allOperators = Operators(operators1, operators2)

  def generateExpressions(size: Int, operators: Operators): Seq[Expression] = {
    if (size == 1) constants
    else {
      (for {
        e1Size <- 1 to (size - 1)
        op1 <- operators.op1s
        e1 <- generateExpressions(e1Size, operators)
      } yield Op1(op1, e1)) ++
        (for {
          e1Size <- 1 to size - 2
          e2Size = size - 1 - e1Size
          op2 <- operators.op2s
          e1 <- generateExpressions(e1Size, operators)
          e2 <- generateExpressions(e2Size, operators)
        } yield Op2(op2, e1, e2))
    }
  }

  case class InputOutput(input: Long, output: Long)
  def matchInput(e: Expression, example: InputOutput): Boolean = (eval(e) == example.output)

  def solveTrain(size: Int): Unit = {
    val trainResponse = io.train(size = size)

    val expressions = generateExpressions(size - 1, trainResponse.operators)
    val inputs = 0 until 10
    for {
      e <- expressions
      x <- inputs
    } {
      val program = Lambda("x", e)
      println(s"program ${stringify(program)} input: ${x} -> ${applyFunction(program, x)}")
    }

  }

}

object BVMain extends App {
  import bv._
  // generateExpressions(4, allOperators) map stringify foreach println

  solveTrain(4)

  //  val program = "(plus 1 (plus 0 1))"
  //  println(parse(program).get)
  //  println(stringify(parse(program).get))

}