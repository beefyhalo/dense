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
object Examples {
  import dense.{ :: => _, _ }, Dense.{ _0, _1 }, ops.{ Succ, Sum, Mod }
  import shapeless.{ HList, HNil, ::, DepFn0, Witness }

  trait IfMultiple[N <: Dense, M <: HList] { type Out <: Dense }

  trait LowPriorityIfMultiple {
    type Aux[N <: Dense, M <: HList, Out0 <: Dense] = IfMultiple[N, M] { type Out = Out0 }

    implicit def isMultiple1[N <: Dense, H <: Dense, T <: HList](implicit ifMultiple: IfMultiple[N, T]): Aux[N, H :: T, ifMultiple.Out] =
      new IfMultiple[N, H :: T] {
        type Out = ifMultiple.Out
      }
  }

  object IfMultiple extends LowPriorityIfMultiple {
    implicit def ifMultiple0[N <: Dense]: Aux[N, HNil, _0] =
      new IfMultiple[N, HNil] {
        type Out = _0
      }

    implicit def ifMultiple2[N <: Dense, H <: Dense, T <: HList](implicit mod: Mod.Aux[N, H, _0]): Aux[N, H :: T, N] =
      new IfMultiple[N, H :: T] {
        type Out = N
      }
  }

  trait SumOfMultiples[N <: Dense, M <: HList] {
    type Out <: Dense
    def apply(acc: Dense, elem: Dense): Out
  }

  object SumOfMultiples {
    type Aux[N <: Dense, M <: HList, Out0 <: Dense] = SumOfMultiples[N, M] { type Out = Out0 }

    def apply[N <: Dense, M <: HList](implicit som: SumOfMultiples[N, M]): Aux[N, M, som.Out] = som

    implicit def sum0[M <: HList]: Aux[_1, M, _0] =
      new SumOfMultiples[_1, M] {
        type Out = _0
        def apply(acc: Dense, elem: Dense) = _0
      }
    /*
    implicit def sumN[P <: Dense: IsDCons, M <: HList, NV <: Dense, PT <: Dense, NT <: Dense](implicit im: IfMultiple.Aux[P, M, NV],
                                                                                              som: Aux[P, M, PT],
                                                                                              sum: Sum.Aux[NV, PT, NT],
                                                                                              succ: Succ[P]): Aux[succ.Out, M, NT] =
      new SumOfMultiples[succ.Out, M] {
        type Out = NT
        def apply(acc: Dense, elem: Dense) = acc + elem
      }
      */
  }
}
object Fib {

  import dense._, Dense._, ops.{ Pred, Sum, ToInt }

  trait Fib[A <: Dense, B <: Dense]

  object Fib {
    def apply[Out <: Dense](n: Dense)(implicit f: Fib[n.N, Out], i: ToInt[Out]) = i()
    implicit val f0 = new Fib[_0, _0] {}
    implicit val f1 = new Fib[_1, _1] {}

    implicit def f2[A <: Dense, P <: Dense, P2 <: Dense, F <: Dense, F2 <: Dense](implicit p: Pred.Aux[A, P],
                                                                                  p2: Pred.Aux[P, P2],
                                                                                  f: Fib[P, F],
                                                                                  f2: Fib[P2, F2],
                                                                                  sum: Sum[F, F2]): Fib[A, sum.Out] =
      new Fib[A, sum.Out] {}
  }

  def wit[Out <: Dense](n: Dense)(implicit f: Fib[n.N, Out], w: shapeless.Witness.Aux[Out]) = w.value

  implicitly[Fib[_0, _0]]
  implicitly[Fib[_1, _1]]
  implicitly[Fib[_2, _1]]
  implicitly[Fib[_3, _2]]
  implicitly[Fib[_4, _3]]
  implicitly[Fib[_5, _5]]
  implicitly[Fib[_6, _8]]
  implicitly[Fib[_7, _13]]

  Fib(7)

  //wit(7)

}

object PrintRange {
  import dense._, Dense.{ _0, _1, _2 }, ops.Pred

  trait Range[A <: Dense]

  object Range {
    def apply[A <: Dense](a: A)(implicit r: Range[A]) = r
    implicit val r0 = new Range[_0] {}
    implicit def r1[A <: Dense, B <: Dense](implicit p: Pred.Aux[A, B], r: Range[B]): Range[A] = new Range[A] {}
  }
  Range(_0)
  Range(_1)
  Range(_2)
}

object GCDExamples {

  import dense._, Dense._, ops._, syntax._
  import shapeless.Witness
  import shapeless.test.typed

  trait GCD[X <: Dense, Y <: Dense] { type Out <: Dense }

  object GCD {
    def gcd[N <: Dense](x: Dense, y: Dense)(implicit gcd: Aux[x.N, y.N, N], wn: Witness.Aux[N]): N = wn.value

    type Aux[X <: Dense, Y <: Dense, Z <: Dense] = GCD[X, Y] { type Out = Z }

    implicit def gcd0[X <: Dense]: Aux[X, X, X] = new GCD[X, X] { type Out = X }
    implicit def gcd1[X <: Dense, Y <: Dense, Z <: Dense, Out0 <: Dense](implicit ev0: LT[X, Y],
                                                                         ev1: Diff.Aux[Y, X, Z],
                                                                         ev2: Aux[X, Z, Out0]): Aux[X, Y, Out0] =
      new GCD[X, Y] { type Out = Out0 }
    implicit def gcd2[X <: Dense, Y <: Dense, Out0 <: Dense](implicit ev0: LT[Y, X],
                                                             ev1: Aux[Y, X, Out0]): Aux[X, Y, Out0] =
      new GCD[X, Y] { type Out = Out0 }
  }

  import GCD._
  /*
  val g1 = gcd(_2, _3)
  typed[_1](g1)
  
  val g2 = gcd(4, 10)
  typed[_2](g2)

  val g3 = gcd(15, 6)
  typed[_3](g3)

  val g4 = gcd(8, 12)
  typed[_4](g4)
  */
}
