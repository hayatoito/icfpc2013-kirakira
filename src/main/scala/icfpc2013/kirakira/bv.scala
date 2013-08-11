package icfpc2013.kirakira

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import java.math.BigInteger

object bv {

  implicit class RichLong(val n: Long) extends AnyVal {
    def toHex: String = f"0x${n}%016x"
  }

  implicit class RichString(val s: String) extends AnyVal {
    def asLong: Long = {
      assert(s.take(2) == "0x")
      new BigInteger(s.drop(2), 16).longValue()
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
    lazy val if0 = "(" ~ "if0" ~ expr ~ expr ~ expr ~ ")" ^^ { case "(" ~ "if0" ~ e0 ~ e1 ~ e2 ~ ")" => If(e0, e1, e2) }
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
    case If(e0, e1, e2) => s"(if0 ${stringify(e0)} ${stringify(e1)} ${stringify(e2)})"
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

  case class Operators(op1s: List[Op1Symbol], op2s: List[Op2Symbol])

  val allOperators = Operators(operators1, operators2)
  def constants = List(ZERO, ONE)

  def generateExpressions(size: Int, operators: Operators, usedIds: Set[String]): Seq[Expression] = {
    if (size == 0) throw BVError()
    else if (size == 1) constants ++ (usedIds.map(id => Id(id)))
    else {
      (for {
        op1 <- operators.op1s
        e1 <- generateExpressions(size - 1, operators, usedIds)
      } yield Op1(op1, e1)) ++
        (for {
          size1 <- 1 to (size - 1) / 2
          size2 = size - 1 - size1
          op2 <- operators.op2s
          e1 <- generateExpressions(size1, operators, usedIds)
          e2 <- generateExpressions(size2, operators, usedIds)
        } yield Op2(op2, e1, e2)) ++
        (for {
          size1 <- 1 to (size - 3)
          size2 <- 1 to (size - 2 - size1)
          size3 = size - 1 - size1 - size2
          e1 <- generateExpressions(size1, operators, usedIds)
          e2 <- generateExpressions(size2, operators, usedIds)
          e3 <- generateExpressions(size3, operators, usedIds)
        } yield If(e1, e2, e3)) ++
        (for {
          size1 <- 1 to (size - 3)
          size2 <- 1 to (size - 2 - size1)
          size3 = size - 1 - size1 - size2
          id1 = "x" + usedIds.size
          id2 = "x" + (usedIds.size + 1)
          e1 <- generateExpressions(size1, operators, usedIds)
          e2 <- generateExpressions(size2, operators, usedIds)
          e3 <- generateExpressions(size3, operators, usedIds ++ Set(id1, id2))
        } yield Fold(e1, e2, id1, id2, e3))

    }
  }

  case class InputOutput(input: Long, output: Long)

  def matchInput(e: Expression, example: InputOutput): Boolean = (eval(e) == example.output)

  def sleep(): Unit = { Thread.sleep(5000) }

  def tachikoma(): Unit = {
    val problems = io.myproblems.filterNot(p => p.solved).sorted
    for {
      problem <- problems.take(10)
    } {
      val inputs = (-128L until 128L)
      sleep()
      val outputs = io.eval(Some(problem.id), None, inputs.map(n => n.toHex).toList).
        outputs.map(x => x.asLong)

      val expressions = generateExpressions(problem.size - 1, io.convert(problem.operators), Set("x"))
      val inputOutputs = inputs.zip(outputs) map (x => InputOutput(x._1, x._2))
      val correctPrograms =
        for {
          e <- expressions
          program = Lambda("x", e)
          if inputOutputs.forall { case InputOutput(input, output) => applyFunction(program, input) == output }
        } yield program

      correctPrograms foreach (x => println(stringify(x)))

      def tryGuess(ps: List[Expression]): Unit = ps match {
        case x :: xs => {
          sleep()
          val guessResponse = io.guess(problem.id, stringify(x)) // "(lamdba (x) (plus x 1)
          println(guessResponse)
          if (guessResponse.status == "mismatch") tryGuess(xs)
        }
        case Nil => Unit
      }
      tryGuess(correctPrograms.toList)
    }
  }

  def solveTrain(size: Int): Unit = {
    val trainResponse = io.train(size = size)

    val inputs = (-128L until 128L)
    val outputs = io.eval(Some(trainResponse.id), None, inputs.map(n => n.toHex).toList).
      outputs.map(x => x.asLong)

    val expressions = generateExpressions(size - 1, trainResponse.operators, Set("x"))
    val inputOutputs = inputs.zip(outputs) map (x => InputOutput(x._1, x._2))
    val correctPrograms =
      for {
        e <- expressions
        program = Lambda("x", e)
        if inputOutputs.forall { case InputOutput(input, output) => applyFunction(program, input) == output }
      } yield program

    correctPrograms foreach (x => println(stringify(x)))
    println(io.guess(trainResponse.id, stringify(correctPrograms.head)))
  }
}

object BVMain extends App {

  import bv._
  // println(parse("(lambda (x) (and x (if0 (and 1 x) 0 x)))"))

  tachikoma()
  // generateExpressions(4, allOperators) map stringify foreach println
  // solveTrain(size = 5)

  //  for {
  //    e <- generateExpressions(size = 3, allOperators, Set("x"))
  //    x <- 0 until 4
  //  } {
  //    val program = Lambda("x", e)
  //    val out = applyFunction(program, x)
  //    println(s"f(x) = ${stringify(program)}, f(${x}) -> ${out} (${out.toHex})")
  //  }

  // solveTrain(size=4)

  //  val program = "(plus 1 (plus 0 1))"
  //  println(parse(program).get)
  //  println(stringify(parse(program).get))

}