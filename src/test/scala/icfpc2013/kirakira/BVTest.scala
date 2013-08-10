package icfpc2013.kirakira

import org.scalatest.FunSuite
import icfpc2013.kirakira.bv._

class BVTest extends FunSuite {

  test("bv test") {
    assert(1 === 1)
  }

  test("bv eval test") {
    assert(eval(Op2(And, ZERO, ONE)) === 0)
//    assert(eval(Op2(And, ONE, ONE)) === 1)
//    assert(eval(Op2(And, ONE, ZERO)) === 0)
//    assert(eval(Op2(And, ZERO, ZERO)) === 0)
//
//    assert(eval(Op2(Or, ZERO, ONE)) === 1)
//    assert(eval(Op2(Or, ONE, ONE)) === 1)
//    assert(eval(Op2(Or, ONE, ZERO)) === 1)
//    assert(eval(Op2(Or, ZERO, ZERO)) === 0)
//
//
//    assert(eval(Op2(Plus, ZERO, ZERO)) === 0)
//    assert(eval(Op2(Plus, ZERO, ONE)) === 1)
//    assert(eval(Op2(Plus, ONE, ONE)) === 2)
//    
//    assert(eval(If(ZERO, ONE, ZERO)) === 0)
//    assert(eval(If(ONE, ONE, ZERO)) === 1)
    
    // assert(parse("(plus 1 2)") === 3)
  }

}
