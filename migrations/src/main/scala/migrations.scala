import cats.Monoid
import cats.instances.all._
import shapeless._
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist
import shapeless.ops.coproduct



trait Migration[A, B] {
  def apply(original: A): B
}

object Migration {
  def pure[A, B](func: A => B): Migration[A, B] =
    new Migration[A, B] {
      def apply(original: A): B = func(original)
    }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B = migration.apply(a)
  }

  implicit def genericMigration[A, B,
    ARepr <: HList, BRepr <: HList,
    Common <: HList, Added <: HList, Unaligned <: HList](

    implicit
    aGen      : LabelledGeneric.Aux[A, ARepr],
    bGen      : LabelledGeneric.Aux[B, BRepr],
    inter     : hlist.Intersection.Aux[ARepr, BRepr, Common],
    diff      : hlist.Diff.Aux[BRepr, Common, Added],
    monoid    : Monoid[Added],
    prepend   : hlist.Prepend.Aux[Added, Common, Unaligned],
    align     : hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] =
    new Migration[A, B] {
      def apply(a: A): B =
        bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))
    }

  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty = zero

      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((x, y) => HNil)

  implicit def emptyHList[K <: Symbol, H, T <: HList](
    implicit hMonoid: Lazy[Monoid[H]],
    tMonoid: Monoid[T]
  ): Monoid[FieldType[K, H] :: T] =

    createMonoid(
      field[K](hMonoid.value.empty) :: tMonoid.empty)
      {(x, y) =>
//        ???
        field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(x.tail, y.tail)
      }
}


case class SameA(a: String, b: Int, c: Boolean)
case class SameB(a: String, b: Int, c: Boolean)

case class DropFieldA(a: String, b: Int, c: Boolean)
case class DropFieldB(a: String, c: Boolean)

case class AddFieldA(a: String)
case class AddFieldB(a: String, z: Int)

case class ReorderA(a: String, z: Int)
case class ReorderB(z: Int, a: String)

case class KitchenSinkA(a: String, b: Int, c: Boolean)
case class KitchenSinkB(c: Boolean, z: Option[String], a: String)



object Main extends Demo {
  import Migration._

  println(SameA("abc", 123, true).migrateTo[SameB])
  println(DropFieldA("abc", 123, true).migrateTo[DropFieldB])
  println(ReorderA("abc", 123).migrateTo[ReorderB])
  println(AddFieldA("abc").migrateTo[AddFieldB])
  println(KitchenSinkA("abc", 123, true).migrateTo[KitchenSinkB])
}
