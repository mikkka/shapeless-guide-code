import MainSecond.sec1
import shapeless._

//second from tail object

trait Penultimate[L] {
  type Out
  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] { type Out = O }
  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  import shapeless.ops.hlist

  implicit def hlistPenultimate[L <: HList, M <: HList, O](
    implicit
    init: hlist.Init.Aux[L, M],
    last: hlist.Last.Aux[M, O]
  ): Penultimate.Aux[L, O] = new Penultimate[L] {
    type Out = O

    def apply(l: L): O = last.apply(init.apply(l))
  }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out =
      inst.apply(a)
  }

  implicit def genericPenultimate[A, R, O](
    implicit
    generic: Generic.Aux[A, R],
    penultimate: Penultimate.Aux[R, O]
  ): Penultimate.Aux[A, O] = new Penultimate[A] {
    type Out = O

    def apply(x: A): O = penultimate.apply(generic.to(x))
  }
}


object PenultimateDemo extends App {
  import Penultimate._
  val penul = Penultimate[Int :: Int :: String :: HNil]

  println(penul(12 :: 13 :: "kek" ::HNil))
  println((12 :: 42 :: "kek" ::HNil).penultimate)

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  IceCream("xvxcxcv", 1, false).penultimate
  PenultimateOps(IceCream("xvxcxcv", 1, false)).penultimate
}