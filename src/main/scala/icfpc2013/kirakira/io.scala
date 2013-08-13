package icfpc2013.kirakira

import scalaj.http.Http
import scalaj.http.HttpOptions
import scala.io.Source
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject

import icfpc2013.kirakira.basic._

object io {

  val token: String = {
    val source = Source.fromInputStream(this.getClass.getResourceAsStream("token.txt"))
    val tokenValue = source.mkString.trim
    source.close()
    tokenValue
  }

  def serverURL = "http://icfpc2013.cloudapp.net/"
  def requestURL(requestType: String): String = serverURL + requestType + "?auth=" + token

  val windowMills: Long = 20 * 1000
  val windowRequests = 4
  var requestTimes: List[Long] = Nil

  def maybeSleep(): Unit = {
    val now: Long = System.currentTimeMillis
    if (requestTimes.size >= windowRequests) {
      val diff = now - requestTimes.last
      if (diff < windowMills) {
        val sleepTime = requestTimes.last + windowMills - now + 200
        println(s"Sleeping ${sleepTime}ms for the next request window slot.")
        Thread.sleep(sleepTime)
      }
    }
  }

  def post(url: String, data: String): Any = {
    maybeSleep()
    // println(s"posting url: ${url}, data: ${data}")
    val response = Http.postData(url, data).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
    requestTimes = (System.currentTimeMillis :: requestTimes).take(windowRequests)
    // println(s"response: ${response}")
    JSON.parseFull(response).get
  }

  def asInt(m: Map[String, Any], key: String): Int = m(key).asInstanceOf[Double].toInt
  def asLong(m: Map[String, Any], key: String): Long = m(key).asInstanceOf[Double].toLong
  def asDouble(m: Map[String, Any], key: String): Double = m(key).asInstanceOf[Double]
  def asOptionDouble(m: Map[String, Any], key: String): Option[Double] = m.get(key) match {
    case Some(x) => Some(x.asInstanceOf[Double])
    case None => None
  }
  def asString(m: Map[String, Any], key: String): String = m(key).toString
  def asBoolean(m: Map[String, Any], key: String): Boolean = m.getOrElse(key, false).asInstanceOf[Boolean]
  def asListString(m: Map[String, Any], key: String): List[String] = m(key).asInstanceOf[List[String]]

  def myproblems(): Seq[Problem] = {
    val problems = post(requestURL("myproblems"), "").asInstanceOf[List[Map[String, Any]]]
    problems map { p =>
      Problem(asString(p, "id"), asInt(p, "size"), asListString(p, "operators"),
              asBoolean(p, "solved"), asOptionDouble(p, "timeLeft"))
    }
  }

  case class EvalResponse(outputs: Seq[String])

  def eval(id: Option[String], program: Option[String], arguments: List[String]): EvalResponse = {
    val json: String = id match {
      case Some(id) => JSONObject(Map("id" -> id, "arguments" -> JSONArray(arguments))).toString
      case None => JSONObject(Map("program" -> program.get, "arguments" -> JSONArray(arguments))).toString
    }
    val response = post(requestURL("eval"), json).asInstanceOf[Map[String, Any]]
    if (response("status") == "ok") EvalResponse(asListString(response, "outputs"))
    else {
      println("error: " + response("message"))
      throw BVError()
    }
  }

  case class GuessResponse(status: String, input: String = "", answer: String = "",
    myAnswer: String = "", message: String = "", lightning: Boolean)

  def guess(id: String, program: String): GuessResponse = {
    val json: String = JSONObject(Map("id" -> id, "program" -> program)).toString
    val response = post(requestURL("guess"), json).asInstanceOf[Map[String, Any]]
    if (response("status") == "win") {
      GuessResponse(status = "win", lightning = asBoolean(response, "lightning"))
    } else if (response("status") == "mismatch") {
      val values = asListString(response, "values")
      GuessResponse(status = "mismatch", input = values(0), answer = values(1), myAnswer = values(2), lightning = asBoolean(response, "lightning"))
    } else {
      GuessResponse(status = "error", message = asString(response, "message"), lightning = asBoolean(response, "lightning"))
    }
  }

  def train(size: Option[Int], operators: List[String] = Nil): TrainProblem = {
    val json: String = size match {
      case Some(n) => JSONObject(Map("size" -> n, "operators" -> JSONArray(operators))).toString
      case None => JSONObject(Map("operators" -> JSONArray(operators))).toString
    }
    val response = post(requestURL("train"), json).asInstanceOf[Map[String, Any]]
    TrainProblem(challenge = asString(response, "challenge"), id = asString(response, "id"), size = asInt(response, "size"),
      operators = asListString(response, "operators"))
  }
}
