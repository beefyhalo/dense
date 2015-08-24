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

package object syntax {

  implicit class DenseOps[N <: Dense](val n: N) extends AnyVal {
    import ops._

    def ::[H <: Digit](h: H): H :: N = dense.::(h, n)

    def safe_::[H <: Digit](h: H)(implicit sc: SafeCons[H, N]): sc.Out = sc(h, n)

    def toInt(implicit toIntN: ToInt[n.N]): Int = Dense.toInt(n)

    def succ(implicit s: Succ[N]): s.Out = s(n)

    def pred(implicit p: Pred[N]): p.Out = p(n)

    def digit(implicit c: IsDCons[N]): c.H = c.digit(n)

    def tail(implicit c: IsDCons[N]): c.T = c.tail(n)

    def shiftLeft(implicit sl: ShiftLeft[N]): sl.Out = sl(n)

    def shiftRight(implicit sr: ShiftRight[N]): sr.Out = sr(n)

    def +[B <: Dense](b: B)(implicit s: Sum[N, B]): s.Out = s(n, b)

    def -[B <: Dense](b: B)(implicit d: Diff[N, B]): d.Out = d(n, b)

    def *[B <: Dense](b: B)(implicit p: Prod[N, B]): p.Out = p(n, b)

    def ^[B <: Dense](b: B)(implicit e: Exp[N, B]): e.Out = e(n, b)

    def /[B <: Dense](b: B)(implicit d: Div[N, B]): d.Out = d(n, b)

    def %[B <: Dense](b: B)(implicit m: Mod[N, B]): m.Out = m(n, b)

    def <[B <: Dense](b: B)(implicit lt: LT[N, B]) = ()

    def <=[B <: Dense](b: B)(implicit lte: LTEq[N, B]) = ()

    def min[B <: Dense](b: B)(implicit c: Compare[N, B]): c.Min = c.min(n, b)

    def max[B <: Dense](b: B)(implicit c: Compare[N, B]): c.Max = c.max(n, b)
  }
}