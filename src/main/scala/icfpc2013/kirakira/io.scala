package icfpc2013.kirakira

import scalaj.http.Http
import scalaj.http.HttpOptions
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONArray

object io {

  case class BVIOException() extends Exception

  val token = "0037R3ngJEM1MscklcXGMmH06VGq634dDm3rKQ4IvpsH1H"

  //  def serverURL = "http://icfpc2013.cloudapp.net/train"
  def serverURL = "http://icfpc2013.cloudapp.net/"
  def requestURL(requestType: String): String = serverURL + requestType + "?auth=" + token

  // http://icfpc2013.cloudapp.net/myproblems?auth=0037R3ngJEM1MscklcXGMmH06VGq634dDm3rKQ4IvpsH1H

  def post(url: String, data: String): Any = {
    println(s"url: ${url}, data: ${data}")
    val response = Http.postData(url, data).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
    println(s"response: ${response}")
    JSON.parseFull(response).get
  }

  case class ProgramInfo(id: String, size: Int, operators: Set[String], solved: Boolean, 
      timeLeft: Option[Double]) extends Ordered[ProgramInfo] {
    override def compare(that: ProgramInfo): Int = size - that.size
  }


  def asInt(m: Map[String, Any], key: String): Int = m(key).asInstanceOf[Double].toInt

  def asDouble(m: Map[String, Any], key: String): Double = m(key).asInstanceOf[Double]
  def asOptionDouble(m: Map[String, Any], key: String): Option[Double] = m.get(key) match {
    case Some(x) => Some(x.asInstanceOf[Double])
    case None => None
  }
  def asString(m: Map[String, Any], key: String): String = m(key).toString
  def asSet(m: Map[String, Any], key: String): Set[String] = m(key).asInstanceOf[List[String]].toSet
  def asBoolean(m: Map[String, Any], key: String): Boolean = m.getOrElse(key, false).asInstanceOf[Boolean]
  def asList(m: Map[String, Any], key: String): List[String] = m(key).asInstanceOf[List[String]]

  def myproblems(): Seq[ProgramInfo] = {
    val problems = post(requestURL("myproblems"), "").asInstanceOf[List[Map[String, Any]]]
    problems map { p =>
      ProgramInfo(asString(p, "id"), asInt(p, "size"), asSet(p, "operators"),
        asBoolean(p, "solved"), asOptionDouble(p, "timeLeft"))
    }
  }

  case class EvalRequest(id: Option[String], program: Option[String], arguments: List[String]) {
    def toJSON: String = id match {
      case Some(id) => JSONObject(Map("id" -> id, "arguments" -> JSONArray(arguments))).toString
      case None => JSONObject(Map("program" -> program.get, "arguments" -> JSONArray(arguments))).toString
    }
  }
  case class EvalResponse(outputs: Seq[String])

  def eval(evalRequest: EvalRequest): EvalResponse = {
    val response = post(requestURL("eval"), evalRequest.toJSON).asInstanceOf[Map[String, Any]]
    if (response("status") == "ok")
      EvalResponse(asList(response, "outputs"))
    else {
      println("error: " + response("message"))
      throw BVIOException()
    }
  }

  case class GuessRequest(id: String, program: String) {
    def toJSON: String = JSONObject(Map("id" -> id, "program" -> program)).toString
  }
  // case class GuessResponse(status: String, values: List[String], message: String, lightning: Boolean)
  case class GuessResponse(status: String, input: String = "", answer: String = "",
    actual: String = "",
    message: String = "", lightning: Boolean)

  def guess(guessRequest: GuessRequest): GuessResponse = {
    val response = post(requestURL("guess"), guessRequest.toJSON).asInstanceOf[Map[String, Any]]
    if (response("status") == "win") {
      GuessResponse(status = "win", lightning = asBoolean(response, "lightning"))
    } else if (response("status") == "mismatch") {
      val values = asList(response, "values")
      GuessResponse(status = "mismatch", input = values(0), answer = values(1), actual = values(2),
        lightning = asBoolean(response, "lightning"))
    } else {
      GuessResponse(status = "error", message = asString(response, "message"), lightning = asBoolean(response, "lightning"))
    }
  }
}

object iomain extends App {
  val problems = io.myproblems()
  println(problems)
  problems.sorted foreach println

  // Program(zWqJMDA99HvjBA1VEvQg5Zbc,3,Set(shr16),false,None)

//    val evalResponse = io.eval(io.EvalRequest(None, Some("(lambda (x) (shr1 (plus (plus 1 1) (plus 1 1))))"), 
//        List("0x01", "0x02", "0x03", "0x04")))
//   println(evalResponse)
  
//    val evalResponse = io.eval(io.EvalRequest(Some("zWqJMDA99HvjBA1VEvQg5Zbc"), None, 
//        List("0x01", "0x02", "0x03", "0x04")))
//   println(evalResponse)

  // val guess = io.guess(io.GuessRequest("zWqJMDA99HvjBA1VEvQg5Zbc", "(lambda (x) (shr16 x))"))
  // val guess = io.guess(io.GuessRequest("zWqJMDA99HvjBA1VEvQg5Zbc", "(lambda (x) (shr16 0))"))
  // val guess = io.guess(io.GuessRequest("zWqJMDA99HvjBA1VEvQg5Zbc", "(lambda (x) (shr16 1))"))
  // println(guess)
}