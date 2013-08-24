package icfpc2013.kirakira

import org.scalatest.FunSuite

import icfpc2013.kirakira.bv._
import icfpc2013.kirakira.solver._

class SolverTest extends FunSuite {

  test("parse hex") {
    assert(asLong("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") == -1L)
  }

  test("canPruneBranch") {
    assert(canPruneBranch(Op1(Shr1, ZERO)) === true)
  }
}
