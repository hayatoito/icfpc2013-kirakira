package icfpc2013.kirakira

import scalaj.http.Http
import scalaj.http.HttpOptions
import scala.io.Source

import spray.json._
import spray.json.DefaultJsonProtocol._

import icfpc2013.kirakira.basic._

object io {

  import icfpc2013.kirakira.basic.JsonProtocol._

  val token: String = {
    val source = Source.fromInputStream(getClass.getResourceAsStream("token.txt"))
    val tokenValue = source.mkString.trim
    source.close()
    tokenValue
  }

  def requestURL(requestType: String) = s"http://icfpc2013.cloudapp.net/${requestType}?auth=${token}"

  val windowMills: Long = 20 * 1000
  val windowRequests = 4
  var requestTimeHistory: List[Long] = Nil

  def maybeSleep(): Unit = {
    val now: Long = System.currentTimeMillis
    if (requestTimeHistory.size >= windowRequests) {
      val diff = now - requestTimeHistory.last
      if (diff < windowMills) {
        val sleepTime = requestTimeHistory.last + windowMills - now + 200
        println(s"Sleeping ${sleepTime}ms for the next request window slot.")
        Thread.sleep(sleepTime)
      }
    }
  }

  case class PostResponse(responseCode: Int, body: Option[String])

  val SUCCESS = 200
  val TOO_MANY_REQUESTS = 429

  def post(url: String, data: String): PostResponse = {
    maybeSleep()
    requestTimeHistory = (System.currentTimeMillis :: requestTimeHistory).take(windowRequests)
    val httpPost = Http.postData(url, data).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000))
    def myConnectionProcessor(conn: java.net.HttpURLConnection): (Int, String, Option[String]) = conn.getResponseCode match {
      case SUCCESS => (SUCCESS, conn.getResponseMessage, Some(Http.tryParse(conn.getInputStream, Http.readString)))
      case x => (x, conn.getResponseMessage, None)
    }
    httpPost.process(myConnectionProcessor) match {
      case (SUCCESS, responseMessage, body) => PostResponse(SUCCESS, body)
      case (TOO_MANY_REQUESTS, responseMessage, body) => {
        println(s"${TOO_MANY_REQUESTS} ${responseMessage}. Try again...")
        post(url, data)
      }
      case (responseCode, responseMessage, body) => {
        println(s"${responseCode} ${responseMessage}.")
        PostResponse(responseCode, body)
      }
    }
  }

  def myproblems(): Seq[Problem] = post(requestURL("myproblems"), "") match {
    case PostResponse(SUCCESS, Some(body)) => body.asJson.convertTo[Seq[Problem]]
    case PostResponse(_, _) => throw BVError()
  }

  def eval(evalRequest: EvalRequest): Response[EvalResponse] = post(requestURL("eval"), evalRequest.toJson.toString) match {
    case PostResponse(SUCCESS, Some(body)) => Response(SUCCESS, Some(body.asJson.convertTo[EvalResponse]))
    case PostResponse(responseCode, optionBody) => Response(responseCode, None, optionBody)
  }

  def guess(guess: Guess): Response[GuessResponse] = post(requestURL("guess"), guess.toJson.toString) match {
    case PostResponse(SUCCESS, Some(body)) => Response(SUCCESS, Some(body.asJson.convertTo[GuessResponse]))
    case PostResponse(responseCode, optionBody) => Response(responseCode, None, optionBody)
  }

  def train(trainRequest: TrainRequest): Response[TrainingProblem] = post(requestURL("train"), trainRequest.toJson.toString) match {
    case PostResponse(SUCCESS, Some(body)) => Response(SUCCESS, Some(body.asJson.convertTo[TrainingProblem]))
    case PostResponse(responseCode, optionBody) => Response(responseCode, None, optionBody)
  }
}
