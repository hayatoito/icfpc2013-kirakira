package icfpc2013.kirakira

import scala.collection.mutable

import icfpc2013.kirakira.io.protocol._
import icfpc2013.kirakira.bv._

object solver {

  def toHex(n: Long): String = f"0x${n}%016x"

  def asLong(s: String): Long = {
    assert(s.take(2) == "0x")
    new java.math.BigInteger(s.drop(2), 16).longValue()
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
  val idx = Id("x")
  val idy = Id("y")
  val allIds = List(idx, idy)

  def solveProblem(problem: Problem): Boolean = {
    println("Problem: " + problem)
    val cache = mutable.Map.empty[(Int, List[Id]), Seq[Expression]]
    val operators = toOperators(problem.operators)

    def generateExpressions(size: Int, usedIds: List[Id]): Seq[Expression] = {
      cache.getOrElseUpdate((size, usedIds),
        if (size == 1) constants ++ usedIds
        else {
          (generateOp1(size, usedIds) ++
            generateOp2(size, usedIds) ++
            (if (operators.if0) generateIf0(size, usedIds) else Nil) ++
            (if (operators.fold) generateFold(size, usedIds) else Nil)).filterNot(canPruneBranch)
        })
    }

    def generateOp1(size: Int, usedIds: List[Id]): Seq[Expression] = {
      for {
        op1 <- operators.op1s
        e1 <- generateExpressions(size - 1, usedIds)
      } yield Op1(op1, e1)
    }

    def generateOp2(size: Int, usedIds: List[Id]): Seq[Expression] = {
      for {
        op2 <- operators.op2s
        size1 <- 1 to (size - 1) / 2
        size2 = size - 1 - size1
        e1 <- generateExpressions(size1, usedIds)
        e2 <- generateExpressions(size2, usedIds)
      } yield Op2(op2, e1, e2)
    }

    def generateIf0(size: Int, usedIds: List[Id]): Seq[Expression] = {
      for {
        size1 <- 1 to (size - 3)
        size2 <- 1 to (size - 2 - size1)
        size3 = size - 1 - size1 - size2
        e1 <- generateExpressions(size1, usedIds)
        e2 <- generateExpressions(size2, usedIds)
        e3 <- generateExpressions(size3, usedIds)
      } yield If0(e1, e2, e3)
    }

    def generateFold(size: Int, usedIds: List[Id]): Seq[Expression] = {
      for {
        size1 <- 1 to (size - 4)
        size2 <- 1 to (size - 3 - size1)
        size3 = size - 2 - size1 - size2
        e1 <- generateExpressions(size1, usedIds)
        e2 <- generateExpressions(size2, usedIds)
        e3 <- generateExpressions(size3, allIds)
      } yield Fold(e1, e2, "x", "y", e3)
    }

    def generateTfold(size: Int, usedIds: List[Id]): Seq[Expression] = {
      for {
        size1 <- 1 to (size - 4)
        e <- generateExpressions(size1, allIds)
      } yield Fold(idx, ZERO, "x", "y", e)
    }

    case class InputOutput(input: Long, output: Long) {
      def satisfiedWith(e: Expression): Boolean = evalAsLambda(e, input) == output
    }

    def iterativeSizeLimit =
      (if (operators.fold) 11
      else if (operators.tfold) 13
      else 11).min(problem.size)

    val inputs = (-128L until 128L)

    val outputs: Seq[Long] = io.eval(EvalRequest(Some(problem.id), None, inputs.map(n => toHex(n)).toList)) match {
      case Some(EvalResponse(_, Some(outputs), _)) => outputs.map(x => asLong(x))
      case _ => return false
    }

    val examples = inputs.zip(outputs) map (x => InputOutput(x._1, x._2))

    def generateFunction: (Int, List[Id]) => Seq[Expression] = if (operators.tfold) generateTfold else generateExpressions

    def filterPrograms(expressions: Seq[Expression], examples: Seq[InputOutput]): Seq[Expression] =
      expressions.filter(e => examples.forall { _.satisfiedWith(e) }).toList

    def tryGuess(expressions: List[Expression], examples: List[InputOutput], size: Int): Boolean = expressions match {
      case e :: es => {
        println(s"  Found candidates with size=${size + 1}. # of candidates: ${expressions.size}.")
        expressions.take(10) foreach { e => println(s"    ${stringify(lambda(e))}") }
        if (expressions.size > 10) println("    ...")
        val guessResponse = io.guess(Guess(problem.id, stringify(lambda(e))))
        println(guessResponse)
        guessResponse match {
          case Some(GuessResponse("win", None, _, _)) => true
          case Some(GuessResponse("mismatch", Some(List(input, answer, actual)), _, _)) => {
            val counterExample = InputOutput(asLong(input), asLong(answer))
            val filtered = filterPrograms(es, List(counterExample))
            println(s"Filtered:  ${es.size + 1} -> ${filtered.size}")
            tryGuess(filtered.toList, counterExample :: examples, size)
          }
          case _ => false
        }
      }
      case Nil =>
        if (size + 2 > iterativeSizeLimit) false
        else {
          println(s"Trying to find candidates with size=${size + 2}...")
          tryGuess(filterPrograms(generateFunction(size + 1, List(idx)), examples).toList, examples, size + 1)
        }
    }
    tryGuess(Nil, examples.toList, 0)
  }

  case class WinLose(win: Int, lose: Int)

  case class Stats(name: String) {
    import collection.immutable.SortedMap

    val stats = mutable.Map.empty[Int, WinLose]
    def sortedStats = SortedMap(stats.toSeq: _*)
    def statsFor(problemSize: Int) = stats.getOrElseUpdate(problemSize, WinLose(0, 0))

    def recordWin(problemSize: Int) = {
      println(s"Win for (size=${problemSize}, type=${name})")
      val wl = statsFor(problemSize)
      stats(problemSize) = wl.copy(win = wl.win + 1)
    }

    def recordLose(problemSize: Int) = {
      println(s"Lose for (size=${problemSize}, type=${name})")
      val wl = statsFor(problemSize)
      stats(problemSize) = wl.copy(lose = wl.lose + 1)
    }

    def printStats(): Unit = {
      println(s"Stats (${name}):")
      for ((size, WinLose(win, lose)) <- sortedStats) {
        println(f"   (size:${size}%2d) Win:${win}%3d, Lose:${lose}%3d, WinRatio: ${1.0 * win / (win + lose)}%2.2f")
      }
    }
  }

  def training(): Unit = {
    val statsList = List(Stats("simple"), Stats("fold"), Stats("tfold"))
    val random = new scala.util.Random

    def solve(): Unit = {
      println("-" * 64)
      statsList foreach { s => s.printStats() }
      println("-" * 64)
      val problemType = random.nextInt(3)
      val operators = List(Nil, List("fold"), List("tfold"))(problemType)
      val stats = statsList(problemType)
      io.train(TrainRequest(size = None, operators = Some(operators))) match {
        case Some(TrainingProblem(challenge, id, size, operators)) => {
          println(s"Training problem: (type=${stats.name}, size=${size}, challenge=${challenge})")
          if (solveProblem(Problem(id = id, size = size, operators = operators, solved = None, timeLeft = None))) stats.recordWin(size)
          else stats.recordLose(size)
          solve()
        }
        case _ => throw BVError()
      }
    }
    solve()
  }

  def availableProblems: Seq[Problem] = io.myproblems match {
    case Some(problems) => problems.filter(p => p.solved.isEmpty).filterNot(p => p.timeLeft.getOrElse(1.0) == 0.0).sortBy(x => x.size + x.operators.size)
    case None => Nil
  }

  def tachikoma(): Unit = {
    def solve(problems: List[Problem], win: Int, lose: Int): Unit = {
      println(s"Remaining problems: ${problems.size}, win: ${win}, lose: ${lose}")
      problems match {
        case x :: xs => if (solveProblem(x)) solve(xs, win + 1, lose) else solve(xs, win, lose + 1)
        case Nil => println("Finished")
      }
    }
    solve(availableProblems.toList, 0, 0)
  }

}

object TrainingSolver extends App {
  solver.training()
}

object ContestSolver extends App {
  solver.tachikoma()
}
