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

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Binary Digit ADT
 */
sealed trait Digit
case object Zero extends Digit { override def toString = "0" }
case object One extends Digit { override def toString = "1" }

/**
 * Base trait for type-level natural numbers.
 */
sealed trait Dense extends Product with Serializable { type N <: Dense }

/**
 * Encoding of zero. Analogous to Nil.
 */
sealed trait DNil extends Dense { type N = DNil }

/**
 * DNil value singleton
 */
case object DNil extends DNil

/**
 * Non-zero natural number encoded as a type-level linked list of bits.
 */
final case class ::[+H <: Digit, T <: Dense](digit: H, tail: T) extends Dense {
  type N = digit.type :: tail.N

  override def toString = {
    @tailrec def toString(n: Dense, acc: String): String = n match {
      case DNil   => acc
      case h :: t => toString(t, h.toString + acc)
    }
    toString(tail, digit.toString)
  }
}

/**
 * Type level encoding of the natural numbers as a linked list of bits.
 */
object Dense extends Denses {
  import ops.ToInt

  def apply(i: Int): Dense = macro DenseMacros.materializeWidened

  def toInt[N <: Dense](implicit toIntN: ToInt[N]) = toIntN()

  def toInt(n: Dense)(implicit toIntN: ToInt[n.N]) = toIntN()

  implicit def materialize(i: Int): Dense = macro DenseMacros.materializeSingleton
}

object DenseMacros {
  def mkDenseTpt(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = {
    import collection.mutable.ListBuffer
    import c.universe._

    val n = i.tree match {
      case Literal(Constant(n: Int)) => n
      case _                         => c.abort(c.enclosingPosition, s"Expression ${i.tree} does not evaluate to an Int constant")
    }

    if (n < 0) c.abort(c.enclosingPosition, s"A Dense cannot represent $n")

    val dconsTpe = typeOf[::[_, _]].typeConstructor
    val (dnilTpe, zeroTpe, oneTpe) = (typeOf[DNil], typeOf[Zero.type], typeOf[One.type])

    @tailrec
    def mkDenseTpt(n: Int)(acc: ListBuffer[Digit]): ListBuffer[Digit] =
      (n / 2, n % 2) match {
        case (0, 0) => acc
        case (q, 0) => mkDenseTpt(q)(acc :+ Zero)
        case (q, 1) => mkDenseTpt(q)(acc :+ One)
      }

    mkDenseTpt(n)(new ListBuffer[Digit]).foldRight((dnilTpe, q"_root_.dense.DNil: $dnilTpe": Tree)) {
      case (Zero, (accTpe, accTree)) => (appliedType(dconsTpe, zeroTpe, accTpe), q"_root_.dense.::[$zeroTpe, $accTpe](Zero, $accTree)")
      case (One, (accTpe, accTree))  => (appliedType(dconsTpe, oneTpe, accTpe), q"_root_.dense.::[$oneTpe, $accTpe](One, $accTree)")
    }._2
  }

  def materializeSingleton(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = {
    import c.universe._

    val denseTpt = mkDenseTpt(c)(i)
    val moduleName = TermName(c.freshName("dense_"))

    q"""
      val $moduleName = $denseTpt
      $moduleName
    """
  }

  def materializeWidened(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = {
    import c.universe._
    val denseTpt = mkDenseTpt(c)(i)
    q"$denseTpt"
  }
}