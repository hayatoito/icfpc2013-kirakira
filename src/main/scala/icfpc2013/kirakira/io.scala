package icfpc2013.kirakira

import scala.io.Source

import scalaj.http.Http
import scalaj.http.HttpOptions

import spray.json._
import spray.json.DefaultJsonProtocol._

object io {

  object protocol {
    case class Problem(id: String, size: Int, operators: List[String], solved: Option[Boolean], timeLeft: Option[Double])

    case class EvalRequest(id: Option[String], program: Option[String], arguments: List[String])
    case class EvalResponse(status: String, outputs: Option[List[String]], message: Option[String])

    case class Guess(id: String, program: String)
    case class GuessResponse(status: String, values: Option[List[String]], message: Option[String], lightning: Option[Boolean])

    case class TrainRequest(size: Option[Int], operators: Option[List[String]])
    case class TrainingProblem(challenge: String, id: String, size: Int, operators: List[String])
  }

  import protocol._

  val token: String = {
    val source = Source.fromInputStream(getClass.getResourceAsStream("token.txt"))
    val tokenValue = source.mkString.trim
    source.close()
    tokenValue
  }

  def requestURL(requestType: String) = s"http://icfpc2013.cloudapp.net/${requestType}?auth=${token}"

  val SUCCESS = 200
  val TOO_MANY_REQUESTS = 429

  def post[T](url: String, data: String)(implicit m: JsonFormat[T]): Option[T] = {
    val httpPost = Http.postData(url, data).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000))
    def myConnectionProcessor(conn: java.net.HttpURLConnection): (Int, String, Option[String]) = conn.getResponseCode match {
      case SUCCESS => (SUCCESS, conn.getResponseMessage, Some(Http.tryParse(conn.getInputStream, Http.readString)))
      case x => (x, conn.getResponseMessage, None)
    }
    httpPost.process(myConnectionProcessor) match {
      case (SUCCESS, _, Some(body)) => Some(body.asJson.convertTo[T])
      case (TOO_MANY_REQUESTS, responseMessage, _) => {
        println(s"${TOO_MANY_REQUESTS} ${responseMessage}. Try again...")
        Thread.sleep(1000)
        post[T](url, data)
      }
      case (responseCode, responseMessage, _) => {
        println(s"${responseCode} ${responseMessage}.")
        None
      }
    }
  }

  object JsonProtocol extends DefaultJsonProtocol {
    implicit val problemFormat = jsonFormat5(Problem)
    implicit val evalRequestFormat = jsonFormat3(EvalRequest)
    implicit val evalResponseFormat = jsonFormat3(EvalResponse)
    implicit val guessFormat = jsonFormat2(Guess)
    implicit val guessResponseFormat = jsonFormat4(GuessResponse)
    implicit val trainRequestFormat = jsonFormat2(TrainRequest)
    implicit val trainingProblemFormat = jsonFormat4(TrainingProblem)
  }

  import JsonProtocol._

  def myproblems = post[Seq[Problem]](requestURL("myproblems"), "")

  def eval(evalRequest: EvalRequest) = post[EvalResponse](requestURL("eval"), evalRequest.toJson.toString)

  def guess(guess: Guess) = post[GuessResponse](requestURL("guess"), guess.toJson.toString)

  def train(trainRequest: TrainRequest) = post[TrainingProblem](requestURL("train"), trainRequest.toJson.toString)
}
