import shapeless.{::, HList, HNil}

trait NTH[L <: HList] {
  type Out
  def apply(value: L): Out
}

trait NTHn {
  type Aux[L <: HList, O] = NTH[L] {type Out = O}

  def apply[L <: HList](implicit inst: NTH[L]): Aux[L, inst.Out] = inst
}

object NTH1 extends NTHn {
  implicit def idx[A, Rest <: HList]: Aux[A :: Rest, A] =
    new NTH[A :: Rest] {
      type Out = A
      def apply(value: A :: Rest): A = value.head
    }
}

object NTH2 extends NTHn {
  implicit def idx[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new NTH[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): B = value.tail.head
    }
}


object MainNTH extends Demo {

  def n1 = {
    import NTH1._
    NTH1[Int :: Int :: HNil]
  }
  def n2 = {
    import NTH2._
    NTH2[String :: String :: HNil]
  }

  println(n1(12 :: 13 :: HNil): Int)
  println(n2("foo" :: "bar" :: HNil): String)
}