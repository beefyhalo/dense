/*
 * Copyright (c) 2015 Kevin Horlick
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
class DenseSpec extends org.specs2.Specification {

  import shapeless.test._
  import dense._
  import Dense._
  import ops._
  import syntax._

  def is = s2"""

 Dense Specification should
   it is working                 $t1
   really working!               $t2
   really working!               $t3
   sum    works                  $t4
   diff   works                  $diff
   eq     works                  $c
   pred   works                  $t5
   prod   works                  $t6
   div    works                  $t7
   mod    works                  $mod
   exp    works                  $t8
   fromI  works                  $t9
   wit    works                  $w
                                 """

  def t1 = {

    val x = One :: One :: DNil

    //implicitly[Succ.Aux[_3, _4]]

    val x2 = Zero :: Zero :: One :: DNil
    val y = x2.toInt
    y must_== 4
  }

  def t2 = _3.toInt must_== 3
  def t3 = _5.toInt must_== 5

  def diff = {
    val a = _3 - _0
    typed[_3](a)

    val b = _3 - _1
    typed[_2](b)

    val c = _3 - _2
    typed[_1](c)

    val d = _3 - _3
    typed[_0](d)

    ok
  }

  def t4 = {

    val x = _1 + _1
    typed[_2](x)

    val y = _1 + _2
    typed[_3](y)

    val z = _3 + _1
    typed[_4](z)

    val w = _4 + _1
    typed[_5](w)

    ok
  }

  def c = {

    _0 < _1
    _1 < _2
    _1 < _3
    _1 < _4
    LT[_2, _3]
    LT[_2, _4]
    LT[_2, _5]
    LT[_3, _4]
    LT[_3, _5]
    LT[_4, _5]
    LT[_5, _6]
    LT[_5, _7]
    LT[_5, _8]
    LT[_7, _8]
    LT[_10, _12]
    LT[_12, _15]

    LTEq[DNil, _1]
    LTEq[_1, _1]
    LTEq[_1, _2]
    LTEq[_1, _3]
    LTEq[_1, _4]
    LTEq[_2, _3]
    LTEq[_3, _4]
    LTEq[_3, _5]
    LTEq[_3, _6]
    LTEq[_4, _5]
    LTEq[_7, _7]
    LTEq[_7, _8]
    LTEq[_7, _10]

    illTyped("_3 <= _2")
    illTyped("_7 <= _5")
    illTyped("_4 < _2")
    illTyped("_2 < _1")
    illTyped("_3 < _2")
    illTyped("_3 < _2")
    illTyped("_7 < _5")

    val x = _1 min _2
    typed[_1](x)

    val y = _2 min _1
    typed[_1](y)

    ok
  }

  def t5 = {

    val a = _1.pred
    val b = _2.pred
    val c = _3.pred
    val d = _4.pred
    val e = _5.pred

    typed[_0](a)
    typed[_1](b)
    typed[_2](c)
    typed[_3](d)
    typed[_4](e)

    ok
  }

  def t6 = {
    val leftZero = _0 * _1
    typed[_0](leftZero)

    val rightZero = _1 * _0
    typed[_0](rightZero)

    val x = _1 * _1
    typed[_1](x)

    val y = _2 * _2
    typed[_4](y)

    val l3 = _3 * _1
    typed[_3](l3)

    val r3 = _1 * _3
    typed[_3](r3)

    val r4 = _3 * _3
    typed[_9](r4)

    val r6 = _2 * _5
    typed[_10](r6)

    val r5 = _5 * _2
    typed[_10](r5)

    val a = _5 * _7
    val b = _7 * _5
    val c = _7 * _7
    val d = _7 * _8
    val e = _15 * _15

    (a.toInt, b.toInt) must_== (35, 35)
  }

  def t7 = {
    val a = _3 / _1
    typed[_3](a)

    val b = _6 / _3
    typed[_2](b)

    val c = _8 / _2
    typed[_4](c)

    val d = _12 / _3
    typed[_4](d)

    ok
  }

  def t8 = {
    val a = _2 ^ _0
    val b = _2 ^ _1
    val c = _2 ^ _2
    val d = _2 ^ _3
    typed[_1](a)
    typed[_2](b)
    typed[_4](c)
    typed[_8](d)

    ok
  }

  def t9 = {

    val x = Dense(3)
    typed[_3](x)

    val y = Dense(6)
    typed[_6](y)

    def f(n: Dense) = n
    val a: Dense = f(3)

    ok
  }

  def mod = {
    val x = _12 % _2
    typed[_0](x)
    ok
  }

  def w = {
    import shapeless.Witness

    def a[A <: Dense, B <: Dense](a: A)(implicit s: Succ.Aux[A, B]) = s(a)
    val x = a(_3)
    typed[_4](x)

    /*
    def b[B <: Dense](a: Dense)(implicit s: Succ.Aux[a.N, B], w: Witness.Aux[B]) = w.value
    val y = b(3)
    typed[_4](b)
    */
    ok
  }
}
