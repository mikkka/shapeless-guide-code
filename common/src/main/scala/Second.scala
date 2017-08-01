import Second.Aux
import shapeless.{::, HList, HNil}

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] {type Out = O}

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

  implicit def hlistSecond[A, B, Rest <: HList] =
    new Second[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): Out =
        value.tail.head
    }
}


object MainSecond extends Demo {
  val sec1 = Second[Int :: Int :: HNil]
  val sec2 = Second[String :: String :: HNil]

  println(sec1(12 :: 13 :: HNil): Int)
  println(sec2("foo" :: "bar" :: HNil): String)
}