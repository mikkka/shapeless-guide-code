import shapeless.{Generic, HList}
import shapeless.ops.hlist.Last

case class Vec(x: Int, y: Int)
case class Rect(origin: Vec, size: Vec)

def lastField[A, Repr <: HList](input: A)(
  implicit
  gen: Generic.Aux[A, Repr],
  last: Last[Repr]
): last.Out = last.apply(gen.to(input))
