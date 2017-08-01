import shapeless.{Generic, HList, HNil, ::}

object WrappedValue1 {
  def getWrappedValue[A, H](input: A)(
    implicit
    gen: Generic.Aux[A, H :: HNil]
  ): H = gen.to(input).head
}

object WrappedValue2 {
  def getWrappedValue[A, Repr <: HList, H, Tail <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    ev: (H :: Tail) =:= Repr
  ): H = ???
    //gen.to(input).head
}

object WrappedValue3 {
  import shapeless.ops.hlist.IsHCons
  def getWrappedValue[A, Repr <: HList, H](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, H, HNil]
  ): H = gen.to(in).head
}

case class Wrapper(value: Int)

object WrappedValueApp extends App {
//  WrappedValue1.getWrappedValue(Wrapper(42))
  println(WrappedValue3.getWrappedValue(Wrapper(42)))
}
