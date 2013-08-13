package icfpc2013.kirakira

import java.math.BigInteger

object basic {

  case class BVError() extends RuntimeException

  case class Problem(id: String, size: Int, operators: List[String], solved: Boolean,
    timeLeft: Option[Double]) extends Ordered[Problem] {
    def howEasy: Int = size + operators.size
    override def compare(that: Problem): Int = howEasy - that.howEasy
  }

  case class TrainProblem(challenge: String, id: String, size: Int, operators: List[String])

  def toHex(n: Long): String = f"0x${n}%016x"

  def asLong(s: String): Long = {
    assert(s.take(2) == "0x")
    new BigInteger(s.drop(2), 16).longValue()
  }
}
