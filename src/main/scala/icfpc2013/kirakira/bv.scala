package icfpc2013.kirakira

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

import icfpc2013.kirakira.basic._

object bv {

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

  val  operators2 = List(And, Or, Xor, Plus)

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

  val NOTZERO = Op1(Not, ZERO)
  val NOTONE = Op1(Not, ONE)

  def canPruneBranch(e: Expression): Boolean = e match {
    case Op1(Shr1, Constant(_)) => true
    case Op1(Shr4, Constant(_)) => true
    case Op1(Shr16, Constant(_)) => true
    case Op1(Shl1, ZERO) => true
    case Op1(Shr4, Op1(Shr1, _)) => true
    case Op1(Shr16, Op1(Shr1, _)) => true
    case Op1(Shr16, Op1(Shr4, _)) => true
    case Op1(Shr16, Op1(Shr16, _)) => true
    case Op1(Not, Op1(Not, _)) => true
    case Op2(_, Id(_), Constant(_)) => true
    case Op2(Plus, ZERO, _) => true
    case Op2(Plus, _, ZERO) => true
    case Op2(And, ZERO, _) => true
    case Op2(And, _, ZERO) => true
    case Op2(And, NOTZERO, _) => true
    case Op2(And, _, NOTZERO) => true
    case Op2(And, x, y) if x == y => true
    case Op2(And, x, Op2(And, y, _)) if x == y => true
    case Op2(Or, ZERO, _) => true
    case Op2(Or, _, ZERO) => true
    case Op2(Or, NOTZERO, _) => true
    case Op2(Or, _, NOTZERO) => true
    case Op2(Or, ONE, ONE) => true
    case Op2(Or, x, Op2(Or, y, _)) if x == y => true
    case Op2(Or, x, y) if x == y => true
    case Op2(Xor, ZERO, _) => true
    case Op2(Xor, _, ZERO) => true
    case Op2(Xor, NOTZERO, _) => true
    case Op2(Xor, _, NOTZERO) => true
    case Op2(Xor, ONE, ONE) => true
    case Op2(Xor, x, y) if x == y => true
    case Op2(Xor, x, Op2(Xor, y, _)) if x == y => true
    case If0(ZERO, _, _) => true
    case If0(ONE, _, _) => true
    case If0(NOTZERO, _, _) => true
    case If0(NOTONE, _, _) => true
    case If0(Op1(Shl1, Constant(_)), _, _) => true
    case If0(_, x, y) if x == y => true
    case _ => false
  }

  case class Operators(op1s: List[Op1Symbol], op2s: List[Op2Symbol], if0: Boolean, fold: Boolean, tfold: Boolean)

  def toOperators(operators: List[String]) =
    Operators(operators1.filter(op => operators.contains(op.symbol)),
              operators2.filter(op => operators.contains(op.symbol)),
              operators.contains("if0"),
              operators.contains("fold"),
              operators.contains("tfold"))

  val constants = List(ZERO, ONE)

  def solveProblem(problem: Problem): Boolean = {
    println("Problem: " + problem)
    val cache = mutable.Map.empty[(Int, Set[String]), Seq[Expression]]
    val operators = toOperators(problem.operators)

    def generateExpressions(size: Int, usedIds: Set[String]): Seq[Expression] = {
      cache.getOrElseUpdate((size, usedIds),
        (if (size == 1) constants ++ (usedIds.map(id => Id(id)))
        else {
          (for {
            op1 <- operators.op1s
            e1 <- generateExpressions(size - 1, usedIds)
          } yield Op1(op1, e1)) ++
            (for {
              op2 <- operators.op2s
              size1 <- 1 to (size - 1) / 2
              size2 = size - 1 - size1
              e1 <- generateExpressions(size1, usedIds)
              e2 <- generateExpressions(size2, usedIds)
            } yield Op2(op2, e1, e2)) ++
            (if (operators.if0) {
              for {
                size1 <- 1 to (size - 3)
                size2 <- 1 to (size - 2 - size1)
                size3 = size - 1 - size1 - size2
                e1 <- generateExpressions(size1, usedIds)
                e2 <- generateExpressions(size2, usedIds)
                e3 <- generateExpressions(size3, usedIds)
              } yield If0(e1, e2, e3)
            } else Nil) ++
            (if (operators.fold) generateFold(size, usedIds) else Nil)
        }).filterNot(canPruneBranch))
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

    case class InputOutput(input: Long, output: Long) {
      def satisfiedWith(e: Expression): Boolean = evalAsLambda(e, input) == output
    }

    def iterativeSizeLimit =
      (if (operators.fold) 11
       else if (operators.tfold) 13
       else 11).min(problem.size)

    val inputs = (-128L until 128L)
    val outputs = io.eval(Some(problem.id), None, inputs.map(n => toHex(n)).toList).outputs.map(asLong)
    val examples = inputs.zip(outputs) map (x => InputOutput(x._1, x._2))

    def generateFunction: (Int, Set[String]) => Seq[Expression] = if (operators.tfold) generateFold else generateExpressions

    def filterPrograms(expressions: Seq[Expression], examples: Seq[InputOutput]): Seq[Expression] =
      expressions.filter(e => examples.forall { _.satisfiedWith(e) }).toList

    def tryGuess(expressions: List[Expression], examples: List[InputOutput], size: Int): Boolean = expressions match {
      case e :: es => {
        println(s"  Found candidates with size=${size + 1}. # of candidates: ${expressions.size}.")
        expressions.take(10) foreach { e => println(s"    ${stringify(lambda(e))}") }
        if (expressions.size > 10) println("    ...")
        val guessResponse = io.guess(problem.id, stringify(lambda(e)))
        println(guessResponse)
        if (guessResponse.status == "win") true
        else if (guessResponse.status == "mismatch") {
          val counterExample = InputOutput(asLong(guessResponse.input), asLong(guessResponse.answer))
          val filtered = filterPrograms(es, List(counterExample))
          println(s"Filtered:  ${es.size + 1} -> ${filtered.size}")
          tryGuess(filtered.toList, counterExample :: examples, size)
        } else false
      }
      case Nil =>
        if (size + 2 > iterativeSizeLimit) false
        else {
          println(s"Trying to find candidates with size=${size + 2}...")
          tryGuess(filterPrograms(generateFunction(size + 1, Set("x")), examples).toList, examples, size + 1)
        }
    }
    tryGuess(Nil, examples.toList, 0)
  }

  def solveTrainProblem(problem: TrainProblem): Boolean = {
    solveProblem(Problem(id = problem.id, size = problem.size, operators = problem.operators, solved = false, timeLeft = None))
  }

  def training(): Unit = {
    import collection.immutable.SortedMap

    case class WinLose(win: Int = 0, lose: Int = 0)

    case class Stats(name: String) {
      val stats = mutable.Map.empty[Int, WinLose]
      def sortedStats = SortedMap(stats.toSeq: _*)
      def winLose(problemSize: Int) = stats.getOrElseUpdate(problemSize, WinLose())
      def win(problemSize: Int) = winLose(problemSize).win
      def lose(problemSize: Int) = winLose(problemSize).lose

      def recordWin(problemSize: Int) = {
        println(s"Win for (size=${problemSize}, type=${name})")
        val wl = winLose(problemSize)
        stats(problemSize) = wl.copy(win=wl.win + 1)
      }

      def recordLose(problemSize: Int) = {
        println(s"Lose for (size=${problemSize}, type=${name})")
        val wl = winLose(problemSize)
        stats(problemSize) = wl.copy(lose=wl.lose + 1)
      }

      def printStats(): Unit = {
        println(s"Stats (${name}):")
        for ((size, WinLose(win, lose)) <- sortedStats) {
          println(f"   (size:${size}%2d) Win:${win}%3d, Lose:${lose}%3d, WinRatio: ${1.0 * win / (win + lose)}%2.2f")
        }
      }
    }

    val statsList = List(Stats("simple"), Stats("fold"), Stats("tfold"))

    val random = new scala.util.Random

    def solve(): Unit = {
      println("-" * 64)
      statsList foreach { s => s.printStats() }
      println("-" * 64)
      val problemType = random.nextInt(3)
      val operators = List(Nil, List("fold"), List("tfold"))(problemType)
      val stats = statsList(problemType)
      val trainProblem = io.train(size = None, operators = operators)
      println(s"Training problem: (size=${trainProblem.size}, type=${stats.name}, challenge=${trainProblem.challenge})")
      if (solveTrainProblem(trainProblem)) stats.recordWin(trainProblem.size)
      else stats.recordLose(trainProblem.size)
      solve()
    }
    solve()
  }

  def availableProblems: Seq[Problem] =
    io.myproblems.filterNot(p => p.solved).filterNot(p => p.timeLeft.getOrElse(1.0) == 0.0).sorted

  def tachikoma(): Unit = {
    def solve(problems: List[Problem], win: Int, lose: Int): Unit = {
      println(s"Remaining problem = ${problems.size}, win: ${win}, lose: ${lose}")
      problems match {
        case x :: xs => if (solveProblem(x)) solve(xs, win + 1, lose) else solve(xs, win, lose + 1)
        case Nil => println("Finished")
      }
    }
    solve(availableProblems.toList, 0, 0)
  }
}

object BVMain extends App {
  bv.training()
}

object BVContest extends App {
  bv.tachikoma()
}
