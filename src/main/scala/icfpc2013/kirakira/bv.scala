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

  def evalX(e: Expression, x: Long): Long = applyFunction(Lambda("x", e), x)

  object Parser extends JavaTokenParsers {
    lazy val expr: Parser[Expression] = constant | id | if0 | op1 | op2 | fold | lambda
    lazy val constant = wholeNumber ^^ { case num => Constant(num.toLong) }
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
    case Op1(op1, e) => s"(${op1.value} ${stringify(e)})"
    case Op2(op2, e1, e2) => s"(${op2.value} ${stringify(e1)} ${stringify(e2)})"
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

  val ALLONE = Op1(Not, ZERO)

  def redundant(e: Expression): Boolean = e match {
    case Op1(Shr1, ZERO) => true
    case Op1(Shr1, ONE) => true
    case Op1(Shr4, ZERO) => true
    case Op1(Shr4, ONE) => true
    case Op1(Shr16, ZERO) => true
    case Op1(Shr16, ONE) => true
    case Op1(Shl1, ZERO) => true
    case Op1(Not, Op1(Not, _)) => true
    case Op2(Plus, ZERO, _) => true
    case Op2(Plus, _, ZERO) => true
    case Op2(And, ZERO, _) => true
    case Op2(And, _, ZERO) => true
    case Op2(And, x, y) if x == y => true
    case Op2(And, ALLONE, _) => true
    case Op2(And, _, ALLONE) => true
    case Op2(And, x, Op2(And, y, _)) if x == y => true
    case Op2(Or, ZERO, _) => true
    case Op2(Or, _, ZERO) => true
    case Op2(Or, ONE, ONE) => true
    case Op2(Or, ALLONE, _) => true
    case Op2(Or, _, ALLONE) => true
    case Op2(Or, x, Op2(Or, y, _)) if x == y => true
    case Op2(Or, x, y) if x == y => true
    case Op2(Xor, ZERO, _) => true
    case Op2(Xor, _, ZERO) => true
    case Op2(Xor, ONE, ONE) => true
    case Op2(Xor, x, y) if x == y => true
    case Op2(Xor, ALLONE, _) => true
    case Op2(Xor, _, ALLONE) => true
    case Op2(Xor, x, Op2(Xor, y, _)) if x == y => true
    case If0(ZERO, _, _) => true
    case If0(ONE, _, _) => true
    case If0(ALLONE, _, _) => true
    case If0(_, x, y) if x == y => true
    case _ => false
  }

  case class Operators(op1s: List[Op1Symbol], op2s: List[Op2Symbol])

  val allOperators = Operators(operators1, operators2)
  def constants = List(ZERO, ONE)

  case class InputOutput(input: Long, output: Long)

  def solveProblem(problem: io.ProgramInfo): Boolean = {
    println("problem: " + problem)
    val cache = mutable.Map.empty[(Int, Set[String]), Seq[Expression]]
    val operators = io.convert(problem.operators)

    def generateExpressions(size: Int, usedIds: Set[String]): Seq[Expression] = {
      cache.getOrElseUpdate((size, usedIds),
        (if (size == 1) constants ++ (usedIds.map(id => Id(id)))
        else {
          (for {
            op1 <- operators.op1s
            e1 <- generateExpressions(size - 1, usedIds)
          } yield Op1(op1, e1)) ++
            (for {
              size1 <- 1 to (size - 1) / 2
              size2 = size - 1 - size1
              op2 <- operators.op2s
              e1 <- generateExpressions(size1, usedIds)
              e2 <- generateExpressions(size2, usedIds)
            } yield Op2(op2, e1, e2)) ++
            (for {
              size1 <- 1 to (size - 3)
              size2 <- 1 to (size - 2 - size1)
              size3 = size - 1 - size1 - size2
              e1 <- generateExpressions(size1, usedIds)
              e2 <- generateExpressions(size2, usedIds)
              e3 <- generateExpressions(size3, usedIds)
            } yield If0(e1, e2, e3)) ++ generateFold(size, usedIds)
        }).filterNot(redundant))
    }

    def generateFold(size: Int, usedIds: Set[String]): Seq[Expression] = {
      (for {
        size1 <- 1 to (size - 4)
        size2 <- 1 to (size - 3 - size1)
        size3 = size - 2 - size1 - size2
        e1 <- generateExpressions(size1, usedIds)
        e2 <- generateExpressions(size2, usedIds)
        e3 <- generateExpressions(size3, Set("x", "y"))
      } yield Fold(e1, e2, "x", "y", e3))
    }

    val inputs = (-128L until 128L)
    val outputs = io.eval(Some(problem.id), None, inputs.map(n => n.toHex).toList).
      outputs.map(x => x.asLong)
    val inputOutputs = inputs.zip(outputs) map (x => InputOutput(x._1, x._2))

    val expressions =
      for {
        e <- (if (problem.operators.contains("tfold")) generateFold(problem.size - 1, Set("x"))
        else generateExpressions(problem.size - 1, Set("x")))
        if inputOutputs.forall { case InputOutput(input, output) => evalX(e, input) == output }
      } yield e
    expressions.take(20) foreach { e => println(stringify(Lambda("x", e))) }
    println(" candidates size: " + expressions.size)

    def tryGuess(ps: List[Expression]): Boolean = ps match {
      case e :: es => {
        val guessResponse = io.guess(problem.id, stringify(Lambda("x", e)))
        println(guessResponse)
        if (guessResponse.status == "win") {
          println("win\n")
          true
        } else if (guessResponse.status == "mismatch") {
          println("filter: before " + es.size)
          val filtered = es.filter(e => evalX(e, guessResponse.input.asLong) == guessResponse.answer.asLong)
          println("filter: after " + filtered.size)
          tryGuess(filtered)
        } else false
      }
      case Nil => {
        println(" No more candidate")
        false
      }
    }
    tryGuess(expressions.toList)
  }

  def availableProblems: Seq[io.ProgramInfo] =
    io.myproblems.filterNot(p => p.solved).filterNot(p => p.timeLeft.getOrElse(1.0) == 0.0).sorted

  def tachikoma(): Unit = {
    val problems = availableProblems
    println("problems size: " + problems.size)
    problems foreach solveProblem
  }

  def solveTrain(size: Int): Boolean = {
    val train = io.train(size = size)
    solveProblem(io.ProgramInfo(id = train.id, size = train.size, operators = train.operators,
      solved = false, timeLeft = None))
  }
}

object BVMain extends App {
  import bv._
  (0 to 100) forall { i =>
    println("problem: " + i)
    solveTrain(size = 8)
  }
  // tachikoma()
}
