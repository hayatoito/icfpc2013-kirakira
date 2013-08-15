package icfpc2013.kirakira

import java.math.BigInteger

import spray.json._
import spray.json.DefaultJsonProtocol._

object basic {

  case class BVError() extends RuntimeException

  case class Problem(id: String, size: Int, operators: List[String], solved: Option[Boolean], timeLeft: Option[Double]) extends Ordered[Problem] {
    def howEasy: Int = size + operators.size
    override def compare(that: Problem): Int = howEasy - that.howEasy
  }

  case class EvalRequest(id: Option[String], program: Option[String], arguments: List[String])
  case class EvalResponse(status: String, outputs: Option[List[String]], message: Option[String])

  case class Guess(id: String, program: String)
  case class GuessResponse(status: String, values: Option[List[String]], message: Option[String], lightning: Option[Boolean])

  case class TrainRequest(size: Option[Int], operators: Option[List[String]])
  case class TrainingProblem(challenge: String, id: String, size: Int, operators: List[String])

  case class Response[T](code: Int, response: Option[T], errorMessage: Option[String] = None)

  object JsonProtocol extends DefaultJsonProtocol {
    implicit val problemFormat = jsonFormat5(Problem)
    implicit val evalRequestFormat = jsonFormat3(EvalRequest)
    implicit val evalResponseFormat = jsonFormat3(EvalResponse)
    implicit val guessFormat = jsonFormat2(Guess)
    implicit val guessResponseFormat = jsonFormat4(GuessResponse)
    implicit val trainRequestFormat = jsonFormat2(TrainRequest)
    implicit val trainingProblemFormat = jsonFormat4(TrainingProblem)
  }

  def toHex(n: Long): String = f"0x${n}%016x"

  def asLong(s: String): Long = {
    assert(s.take(2) == "0x")
    new BigInteger(s.drop(2), 16).longValue()
  }
}
