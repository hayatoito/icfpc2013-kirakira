package icfpc2013.kirakira

import org.scalatest.FunSuite

import icfpc2013.kirakira.bv._

class BVTest extends FunSuite {

  val N2 = Op2(Plus, ONE, ONE)
  val N3 = Op2(Plus, ONE, N2)
  val N6 = Op1(Shl1, N3)

  test("eval constant") {
    assert(eval(ZERO) === 0)
    assert(eval(ONE) === 1)
  }

  test("eval not") {
    assert(~0 === -1)
    assert(eval(Op1(Not, ZERO)) === -1)
    assert(~1 === -2)
    assert(eval(Op1(Not, ONE)) === -2)
  }

  test("eval shl1") {
    assert(0 << 1 === 0)
    assert(eval(Op1(Shl1, ZERO)) === 0)
    assert(1 << 1 === 2)
    assert(eval(Op1(Shl1, ONE)) === 2)
    assert(2 << 1 === 4)
    assert(eval(Op1(Shl1, N2)) === 4)
    assert(3 << 1 === 6)
    assert(eval(Op1(Shl1, N3)) === 6)
  }

  test("eval shr1") {
    assert(0 >>> 1 === 0)
    assert(eval(Op1(Shr1, ZERO)) === 0)
    assert(1 >>> 1 === 0)
    assert(eval(Op1(Shr1, ONE)) === 0)
    assert(2 >>> 1 === 1)
    assert(eval(Op1(Shr1, N2)) === 1)
    assert(3 >>> 1 === 1)
    assert(eval(Op1(Shr1, N3)) === 1)
  }

  test("eval and") {
    assert(eval(Op2(And, ZERO, ONE)) === 0)
    assert(eval(Op2(And, ONE, ONE)) === 1)
    assert(eval(Op2(And, ONE, ZERO)) === 0)
    assert(eval(Op2(And, ZERO, ZERO)) === 0)

    assert(eval(Op2(And, N2, ONE)) === 0)
    assert(eval(Op2(And, N2, N2)) === 2)
    assert(eval(Op2(And, N2, N3)) === 2)
  }

  test("eval or") {
    assert(eval(Op2(Or, ZERO, ONE)) === 1)
    assert(eval(Op2(Or, ONE, ONE)) === 1)
    assert(eval(Op2(Or, ONE, ZERO)) === 1)
    assert(eval(Op2(Or, ZERO, ZERO)) === 0)
  }

  test("eval xor") {
    assert(eval(Op2(Xor, ZERO, ONE)) === 1)
    assert(eval(Op2(Xor, ONE, ONE)) === 0)
    assert(eval(Op2(Xor, ONE, ZERO)) === 1)
    assert(eval(Op2(Xor, ZERO, ZERO)) === 0)
  }

  test("eval plus") {
    assert(eval(Op2(Plus, ZERO, ZERO)) === 0)
    assert(eval(Op2(Plus, ZERO, ONE)) === 1)
    assert(eval(Op2(Plus, ONE, ONE)) === 2)
  }

  test("eval if") {
    assert(eval(If0(ZERO, ONE, ZERO)) === 1)
    assert(eval(If0(ONE, ONE, ZERO)) === 0)
  }

  test("eval lambda") {
    assert(applyFunction(Lambda("x", Id("x")), 77) === 77)
    assert(applyFunction(Lambda("x", Op2(Plus, Id("x"), ONE)), 3) === 4)
  }

  test("eval fold") {
    val sum = Lambda("x", Fold(Id("x"), ZERO, "y", "z", Op2(Plus, Id("y"), Id("z"))))
    assert(applyFunction(sum, 0x010203) === 6)

    val sumAfterDoubled = Lambda("x", Fold(Id("x"), ZERO, "y", "z", Op2(Plus, Op2(Plus, Id("y"), Id("y")), Id("z"))))
    assert(applyFunction(sumAfterDoubled, 0x010203) === 12)
  }

  test("parser") {
    assert(parse("x") === Id("x"))
    assert(parse("(not 1)") === Op1(Not, ONE))
  }

  test("parser and stringify") {
    assert(stringify(parse("x")) === "x")
    val source = "(lambda (x) (fold x 0 (lambda (y z) (plus y z))))"
    assert(stringify(parse(source)) === source)
  }
}
