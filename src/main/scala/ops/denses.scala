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
package dense
package ops

import shapeless.{ DepFn1, DepFn2 }
import Dense.{ _0, _1 }
import syntax._

/**
 * Type class witnessing that `N` is non-zero and providing access to its digit and tail.
 */
trait IsDCons[N <: Dense] extends Serializable {
  type H <: Digit
  type T <: Dense

  def digit(n: N): H
  def tail(n: N): T
}

object IsDCons {
  type Aux[N <: Dense, H0 <: Digit, T0 <: Dense] = IsDCons[N] {
    type H = H0
    type T = T0
  }

  def apply[N <: Dense](implicit ev: IsDCons[N]): Aux[N, ev.H, ev.T] = ev

  implicit def isDCons[H0 <: Digit, T0 <: Dense]: Aux[H0 :: T0, H0, T0] =
    new IsDCons[H0 :: T0] {
      type H = H0
      type T = T0

      def digit(n: H0 :: T0): H = n.digit
      def tail(n: H0 :: T0): T = n.tail
    }
}

/*
 * Type class supporting the construction of a `Dense` safely by disallowing leading zeros.
 */
trait SafeCons[H <: Digit, T <: Dense] extends DepFn2[H, T] with Serializable { type Out <: Dense }

trait LowPrioritySafeCons {
  type Aux[H <: Digit, T <: Dense, Out0 <: Dense] = SafeCons[H, T] { type Out = Out0 }

  implicit def sc1[H <: Digit, T <: Dense]: Aux[H, T, H :: T] =
    new SafeCons[H, T] {
      type Out = H :: T
      def apply(h: H, t: T) = h :: t
    }
}

object SafeCons extends LowPrioritySafeCons {
  implicit val sc0: Aux[Zero.type, DNil, DNil] =
    new SafeCons[Zero.type, DNil] {
      type Out = DNil
      def apply(h: Zero.type, t: DNil) = DNil
    }
}

/**
 * Type class witnessing that `Out` is `N` shifted to the left by one.
 */
trait ShiftLeft[N <: Dense] extends DepFn1[N] with Serializable { type Out <: Dense }

object ShiftLeft {
  type Aux[N <: Dense, Out0 <: Dense] = ShiftLeft[N] { type Out = Out0 }

  implicit def sl1[T <: Dense](implicit sc: SafeCons[Zero.type, T]): Aux[T, sc.Out] =
    new ShiftLeft[T] {
      type Out = sc.Out
      def apply(n: T) = Zero safe_:: n
    }
}

/**
 * Type class witnessing that `Out` is `N` shifted to the right by one.
 */
trait ShiftRight[N <: Dense] extends DepFn1[N] with Serializable { type Out <: Dense }

object ShiftRight {
  type Aux[N <: Dense, Out0 <: Dense] = ShiftRight[N] { type Out = Out0 }

  implicit val sr0: Aux[_0, _0] =
    new ShiftRight[_0] {
      type Out = _0
      def apply(a: _0) = _0
    }

  implicit def sr1[H <: Digit, T <: Dense]: Aux[H :: T, T] =
    new ShiftRight[H :: T] {
      type Out = T
      def apply(n: H :: T) = n.tail
    }
}

/**
 * Type class supporting computing the successor of `N`.
 */
trait Succ[N <: Dense] extends DepFn1[N] with Serializable { type Out <: Dense }

object Succ {
  type Aux[N <: Dense, Out0 <: Dense] = Succ[N] { type Out = Out0 }

  def apply[N <: Dense](implicit succ: Succ[N]): Aux[N, succ.Out] = succ

  implicit val succ0: Aux[DNil, One.type :: DNil] =
    new Succ[DNil] {
      type Out = One.type :: DNil
      def apply(DNil: DNil) = One :: DNil
    }

  implicit def succ1[T <: Dense]: Aux[Zero.type :: T, One.type :: T] =
    new Succ[Zero.type :: T] {
      type Out = One.type :: T
      def apply(n: Zero.type :: T) = One :: n.tail
    }

  implicit def succ2[T <: Dense, S <: Dense](implicit ev: Aux[T, S],
                                             sl: ShiftLeft[S]): Aux[One.type :: T, sl.Out] =
    new Succ[One.type :: T] {
      type Out = sl.Out
      def apply(n: One.type :: T) = n.tail.succ.shiftLeft
    }
}

/**
 * Type class supporting computing of the predecessor of `N`.
 */
trait Pred[N <: Dense] extends DepFn1[N] with Serializable { type Out <: Dense }

trait LowPriorityPred {
  type Aux[N <: Dense, Out0 <: Dense] = Pred[N] { type Out = Out0 }

  implicit def pred0[T <: Dense](implicit sl: ShiftLeft[T]): Aux[One.type :: T, sl.Out] =
    new Pred[One.type :: T] {
      type Out = sl.Out
      def apply(n: One.type :: T) = n.tail.shiftLeft
    }
}

object Pred extends LowPriorityPred {
  def apply[N <: Dense](implicit pred: Pred[N]): Aux[N, pred.Out] = pred

  implicit val pred1: Aux[_1, _0] =
    new Pred[_1] {
      type Out = _0
      def apply(n: _1) = _0
    }

  implicit def pred2[T <: Dense](implicit p: Pred[T]): Aux[Zero.type :: T, One.type :: p.Out] =
    new Pred[Zero.type :: T] {
      type Out = One.type :: p.Out
      def apply(n: Zero.type :: T) = One :: n.tail.pred
    }
}

/**
 * Type class witnessing that `Out` is the difference of `A` and `B`.
 */
trait Diff[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

trait LowPriorityDiff {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Diff[A, B] { type Out = Out0 }

  implicit def diff1[H <: Digit, T <: Dense, BT <: Dense, O <: Dense](implicit d: Aux[T, BT, O],
                                                                      sl: ShiftLeft[O]): Aux[H :: T, H :: BT, sl.Out] =
    new Diff[H :: T, H :: BT] {
      type Out = sl.Out
      def apply(a: H :: T, b: H :: BT) = (a.tail - b.tail).shiftLeft
    }
}

object Diff extends LowPriorityDiff {
  implicit def diff0[A <: Dense]: Aux[A, _0, A] =
    new Diff[A, _0] {
      type Out = A
      def apply(a: A, b: _0) = a
    }

  implicit def diff1B[A <: Dense]: Aux[A, A, _0] =
    new Diff[A, A] {
      type Out = _0
      def apply(a: A, b: A) = _0
    }

  implicit def diff2[T <: Dense, BT <: Dense](implicit d: Diff[T, BT]): Aux[One.type :: T, Zero.type :: BT, One.type :: d.Out] =
    new Diff[One.type :: T, Zero.type :: BT] {
      type Out = One.type :: d.Out
      def apply(a: One.type :: T, b: Zero.type :: BT) = One :: (a.tail - b.tail)
    }

  implicit def diff3[T <: Dense, BT <: Dense, O <: Dense](implicit d: Diff.Aux[T, BT, O],
                                                          p: Pred[O]): Aux[Zero.type :: T, One.type :: BT, One.type :: p.Out] =
    new Diff[Zero.type :: T, One.type :: BT] {
      type Out = One.type :: p.Out
      def apply(a: Zero.type :: T, b: One.type :: BT) = One :: (a.tail - b.tail).pred
    }
}

/**
 * Type class witnessing that `Out` is the sum of `A` and `B`.
 */
trait Sum[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

trait LowPrioritySum {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Sum[A, B] { type Out = Out0 }

  implicit def sum0[A <: Dense]: Aux[A, _0, A] =
    new Sum[A, _0] {
      type Out = A
      def apply(a: A, b: _0) = a
    }

  implicit def sum1[T <: Dense, BH <: Digit, BT <: Dense](implicit s: Sum[T, BT]): Aux[Zero.type :: T, BH :: BT, BH :: s.Out] =
    new Sum[Zero.type :: T, BH :: BT] {
      type Out = BH :: s.Out
      def apply(a: Zero.type :: T, b: BH :: BT) = b.digit :: (a.tail + b.tail)
    }
}

object Sum extends LowPrioritySum {
  def apply[A <: Dense, B <: Dense](implicit sum: Sum[A, B]): Aux[A, B, sum.Out] = sum

  implicit def sum2[B <: Dense]: Aux[_0, B, B] =
    new Sum[_0, B] {
      type Out = B
      def apply(a: _0, b: B) = b
    }

  implicit def sum3[H <: Digit, T <: Dense, BT <: Dense](implicit s: Sum[T, BT]): Aux[H :: T, Zero.type :: BT, H :: s.Out] =
    new Sum[H :: T, Zero.type :: BT] {
      type Out = H :: s.Out
      def apply(a: H :: T, b: Zero.type :: BT) = a.digit :: (a.tail + b.tail)
    }

  implicit def sum4[T <: Dense, BT <: Dense, R <: Dense, R0 <: Dense](implicit s: Aux[T, BT, R],
                                                                      succ: Succ.Aux[R, R0],
                                                                      sl: ShiftLeft[R0]): Aux[One.type :: T, One.type :: BT, sl.Out] =
    new Sum[One.type :: T, One.type :: BT] {
      type Out = sl.Out
      def apply(a: One.type :: T, b: One.type :: BT) = (a.tail + b.tail).succ.shiftLeft
    }
}

/**
 * Type class witnessing that `Out` is the product of `A` and `B`.
 */
trait Prod[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

trait LowPriorityProd {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Prod[A, B] { type Out = Out0 }

  implicit def prod0b[B <: Dense: IsDCons]: Aux[_0, B, _0] =
    new Prod[_0, B] {
      type Out = _0
      def apply(a: _0, b: B) = _0
    }

  implicit def prod1b[B <: Dense: IsDCons]: Aux[_1, B, B] =
    new Prod[_1, B] {
      type Out = B
      def apply(a: _1, b: B) = b
    }

  trait Prod0[Acc <: Dense, A <: Dense, B <: Dense] {
    type Out <: Dense
    def apply(acc: Acc, a: A, b: B): Out
  }

  object Prod0 {
    import shapeless.Lazy

    type Aux[Acc <: Dense, A <: Dense, B <: Dense, Out0 <: Dense] = Prod0[Acc, A, B] { type Out = Out0 }

    implicit def prod0[Out0 <: Dense, A <: Dense]: Aux[Out0, A, _0, Out0] =
      new Prod0[Out0, A, _0] {
        type Out = Out0
        def apply(acc: Out0, a: A, b: _0) = acc
      }

    implicit def prod1[A <: Dense, SL <: Dense, BT <: Dense, Acc <: Dense](implicit sl: ShiftLeft.Aux[A, SL],
                                                                           prod: Lazy[Prod0[Acc, SL, BT]]): Aux[Acc, A, Zero.type :: BT, prod.value.Out] =
      new Prod0[Acc, A, Zero.type :: BT] {
        type Out = prod.value.Out
        def apply(acc: Acc, a: A, b: Zero.type :: BT) = prod.value(acc, a.shiftLeft, b.tail)
      }

    implicit def prod2[A <: Dense, SL <: Dense, BT <: Dense, Acc <: Dense, Out0 <: Dense, SO <: Dense](implicit sl: ShiftLeft.Aux[A, SL],
                                                                                                       sum: Sum.Aux[Acc, A, SO],
                                                                                                       prod: Lazy[Prod0[SO, SL, BT]]): Aux[Acc, A, One.type :: BT, prod.value.Out] =
      new Prod0[Acc, A, One.type :: BT] {
        type Out = prod.value.Out
        def apply(acc: Acc, a: A, b: One.type :: BT) = prod.value(acc + a, a.shiftLeft, b.tail)
      }
  }
}

object Prod extends LowPriorityProd {
  def apply[A <: Dense, B <: Dense](implicit prod: Prod[A, B]): Aux[A, B, prod.Out] = prod

  implicit def prod0[A <: Dense]: Aux[A, _0, _0] =
    new Prod[A, _0] {
      type Out = _0
      def apply(a: A, b: _0) = _0
    }

  implicit def prod1[A <: Dense: IsDCons]: Aux[A, _1, A] =
    new Prod[A, _1] {
      type Out = A
      def apply(a: A, b: _1) = a
    }

  // XXX
  implicit def prod2[A <: Dense, B0 <: Digit, B1 <: Digit, BT <: Dense, Min <: Dense, Max <: Dense](implicit c: Compare.Aux[A, B0 :: B1 :: BT, Min, Max],
                                                                                                    prod: Prod0[_0, Max, Min]) =
    new Prod[A, B0 :: B1 :: BT] {
      type Out = prod.Out
      def apply(a: A, b: B0 :: B1 :: BT) = prod(_0, a max b, a min b)
    }
}

/**
 * Type class witnessing that `Out` is `A` to the power of `B`.
 */
trait Exp[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

trait LowPriorityExp {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Exp[A, B] { type Out = Out0 }

  implicit def exp2[A <: Dense, BT <: Dense, E <: Dense](implicit e: Aux[A, BT, E],
                                                         p: Prod[E, E]) =
    new Exp[A, Zero.type :: BT] {
      type Out = p.Out
      def apply(a: A, b: Zero.type :: BT) = {
        val x = a ^ b.tail
        x * x
      }
    }

  implicit def exp3[A <: Dense, BT <: Dense, E <: Dense, P <: Dense](implicit e: Aux[A, BT, E],
                                                                     p: Prod.Aux[E, E, P],
                                                                     p2: Prod[P, A]) =
    new Exp[A, One.type :: BT] {
      type Out = p2.Out
      def apply(a: A, b: One.type :: BT) = {
        val x = a ^ b.tail
        x * x * a
      }
    }
}

object Exp extends LowPriorityExp {
  implicit def exp0[A <: Dense] = new Exp[A, _0] {
    type Out = _1
    def apply(a: A, b: _0) = _1
  }

  implicit def exp1[A <: Dense] = new Exp[A, _1] {
    type Out = A
    def apply(a: A, b: _1) = a
  }
}

/**
 * Type class supporting computation of `Quo` and `Rem` the quotient and remainder of `A` and `B`.
 */
trait DivMod[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable {
  type Quo <: Dense
  type Rem <: Dense
  type Out = (Quo, Rem)
}

trait LowPriorityDivMod {
  type Aux[A <: Dense, B <: Dense, Quo0 <: Dense, Rem0 <: Dense] = DivMod[A, B] { type Quo = Quo0; type Rem = Rem0 }

  implicit def div2[H <: Digit, T <: Dense, B <: Dense, Quo0 <: Dense, Rem0 <: Dense](implicit d: Aux[T, B, Quo0, Rem0],
                                                                                      ev: LT[H :: Rem0, B],
                                                                                      sc: SafeCons[H, Rem0],
                                                                                      sl: ShiftLeft[Quo0]): Aux[H :: T, B, sl.Out, sc.Out] =
    new DivMod[H :: T, B] {
      type Quo = sl.Out
      type Rem = sc.Out
      def apply(a: H :: T, b: B) = {
        val (q0, r0) = d(a.tail, b)
        (q0.shiftLeft, a.digit safe_:: r0)
      }
    }

  implicit def div3[H <: Digit, T <: Dense, B <: Dense, Quo0 <: Dense, Rem <: Dense, SL <: Dense, SC <: Dense](implicit d: Aux[T, B, Quo0, Rem],
                                                                                                               ev: LTEq[B, H :: Rem],
                                                                                                               sl: ShiftLeft.Aux[Quo0, SL],
                                                                                                               inc: Succ[SL],
                                                                                                               sc: SafeCons.Aux[H, Rem, SC],
                                                                                                               diff: Diff[SC, B]): Aux[H :: T, B, inc.Out, diff.Out] =
    new DivMod[H :: T, B] {
      type Quo = inc.Out
      type Rem = diff.Out
      def apply(a: H :: T, b: B) = {
        val (q0, r0) = d(a.tail, b)
        val (q, r) = (q0.shiftLeft, a.digit safe_:: r0)
        (q.succ, r - b)
      }
    }
}

object DivMod extends LowPriorityDivMod {
  implicit def div0[B <: Dense: IsDCons]: Aux[_0, B, _0, _0] =
    new DivMod[_0, B] {
      type Quo = _0
      type Rem = _0
      def apply(a: _0, b: B) = (_0, _0)
    }

  implicit def div1[A <: Dense: IsDCons]: Aux[A, _1, A, _0] =
    new DivMod[A, _1] {
      type Quo = A
      type Rem = _0
      def apply(a: A, b: _1) = (a, _0)
    }
}

/**
 * Type class witnessing that `Out` is the quotient of `A` and `B`.
 */
trait Div[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

object Div {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Div[A, B] { type Out = Out0 }

  implicit def div[A <: Dense, B <: Dense](implicit d: DivMod[A, B]) =
    new Div[A, B] {
      type Out = d.Quo
      def apply(a: A, b: B) = d(a, b)._1
    }
}

/**
 * Type class witnessing that `Out` is `A` mod `B`.
 */
trait Mod[A <: Dense, B <: Dense] extends DepFn2[A, B] with Serializable { type Out <: Dense }

object Mod {
  type Aux[A <: Dense, B <: Dense, Out0 <: Dense] = Mod[A, B] { type Out = Out0 }

  implicit def mod[A <: Dense, B <: Dense](implicit d: DivMod[A, B]) =
    new Mod[A, B] {
      type Out = d.Rem
      def apply(a: A, b: B) = d(a, b)._2
    }
}

object C {
  sealed trait Ord
  trait LT extends Ord
  trait EQ extends Ord
  trait GT extends Ord

  trait Comparison[A <: Dense, B <: Dense, Out <: Ord]

  object Comparison {
    import shapeless.=:!=

    implicit val c0 = new Comparison[_0, _0, EQ] {}
    implicit def c1[B <: Dense: IsDCons]: Comparison[_0, B, LT] = new Comparison[_0, B, LT] {}
    implicit def c2[A <: Dense: IsDCons]: Comparison[A, _0, GT] = new Comparison[A, _0, GT] {}

    implicit def c3[H <: Digit, T <: Dense, B <: Digit, BT <: Dense, O <: Ord](implicit c: Comparison[T, BT, O],
                                                                               ev: O =:!= EQ): Comparison[H :: T, B :: BT, O] =
      new Comparison[H :: T, B :: BT, O] {}

    implicit def c4[T <: Dense, BT <: Dense](implicit c: Comparison[T, BT, EQ]): Comparison[One.type :: T, Zero.type :: BT, GT] = new Comparison[One.type :: T, Zero.type :: BT, GT] {}
    implicit def c5[T <: Dense, BT <: Dense](implicit c: Comparison[T, BT, EQ]): Comparison[Zero.type :: T, One.type :: BT, LT] = new Comparison[Zero.type :: T, One.type :: BT, LT] {}
    implicit def c6[H <: Digit, T <: Dense, BT <: Dense, O <: Ord](implicit c: Comparison[T, BT, O]): Comparison[H :: T, H :: BT, O] = new Comparison[H :: T, H :: BT, O] {}
  }
}

/**
 * Type class witnessing that `A` is less than `B`.
 */
trait LT[A <: Dense, B <: Dense] extends Serializable

object LT {
  type <[A <: Dense, B <: Dense] = LT[A, B]
  def apply[A <: Dense, B <: Dense](implicit lt: A < B): A < B = lt
  implicit def lt[A <: Dense, B <: Dense](implicit ev: C.Comparison[A, B, C.LT]): A < B = new <[A, B] {}
}

/**
 * Type class witnessing that `A` is less than or equal to `B`.
 */
trait LTEq[A <: Dense, B <: Dense] extends Serializable

object LTEq {
  type <=[A <: Dense, B <: Dense] = LTEq[A, B]
  def apply[A <: Dense, B <: Dense](implicit lteq: A <= B): A <= B = lteq
  implicit def lteq0[A <: Dense, B <: Dense](implicit ev: C.Comparison[A, B, C.EQ]): A <= B = new <=[A, B] {}
  implicit def lteq1[A <: Dense, B <: Dense](implicit ev: C.Comparison[A, B, C.LT]): A <= B = new <=[A, B] {}
}

/**
 * Type class supporting the computation of `Min` and `Max` of `A` and `B`.
 */
trait Compare[A <: Dense, B <: Dense] extends Serializable {
  type Min <: Dense
  type Max <: Dense

  def min(a: A, b: B): Min
  def max(a: A, b: B): Max
}

object Compare {
  type Aux[A <: Dense, B <: Dense, Min0 <: Dense, Max0 <: Dense] = Compare[A, B] { type Min = Min0; type Max = Max0 }

  def apply[A <: Dense, B <: Dense](implicit c: Compare[A, B]): Aux[A, B, c.Min, c.Max] = c

  implicit def c0[A <: Dense, B <: Dense](implicit ev: LTEq[A, B]): Aux[A, B, A, B] =
    new Compare[A, B] {
      type Min = A
      type Max = B

      def min(a: A, b: B) = a
      def max(a: A, b: B) = b
    }

  implicit def c1[A <: Dense, B <: Dense](implicit ev: LT[B, A]): Aux[A, B, B, A] =
    new Compare[A, B] {
      type Min = B
      type Max = A

      def min(a: A, b: B) = b
      def max(a: A, b: B) = a
    }
}

/**
 * Type class supporting conversion of type-level `Dense`s to value level Ints.
 */
trait ToInt[N <: Dense] extends Serializable {
  def apply(): Int
}

object ToInt {
  def apply[N <: Dense](implicit toInt: ToInt[N]): ToInt[N] = toInt

  implicit val toInt0 = new ToInt[DNil] {
    def apply() = 0
  }

  implicit def toInt1[T <: Dense](implicit i0: ToInt[T]) =
    new ToInt[Zero.type :: T] {
      def apply() = i0() * 2
    }

  implicit def toInt2[T <: Dense](implicit i1: ToInt[T]) =
    new ToInt[One.type :: T] {
      def apply() = i1() * 2 + 1
    }
}
