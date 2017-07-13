import shapeless.{HList, ::, HNil}
import shapeless.Lazy
import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
import shapeless.Generic


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}



object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc

  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

   implicit val stringEnc: CsvEncoder[String] =
     pure(str => List(str))

   implicit val intEnc: CsvEncoder[Int] =
     pure(num => List(num.toString))

  implicit val doubleEnc: CsvEncoder[Double] =
    pure(num => List(num.toString))

   implicit val booleanEnc: CsvEncoder[Boolean] =
     pure(bool => List(if(bool) "yes" else "no"))


  implicit val hnilEncoder: CsvEncoder[HNil] = pure(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList]
              (implicit  hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = {
    pure { case head :: tail =>
        hEncoder.encode(head) ++ tEncoder.encode(tail)
    }
  }

  implicit val cnilEncoder: CsvEncoder[CNil] =
    pure(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct]
              (implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = pure {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }


  implicit def genericEncoder[A, R]
              (implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A] = {
    pure {x =>
      enc.encode(gen.to(x))
    }
  }
}

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

sealed trait Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]


object MainCsv extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")
  }
  
  val shapes: List[Shape] =
    List(
      Rectangle(1, 2),
      Circle(3),
      Rectangle(4, 5),
      Circle(6)
    )

  val optShapes: List[Option[Shape]] =
    List(
      Some(Rectangle(1, 2)),
      Some(Circle(3)),
      None,
      Some(Rectangle(4, 5)),
      Some(Circle(6)),
      None
    )

  println("Shapes " + shapes)
  println("Shapes as CSV:\n" + writeCsv(shapes))
//  println("Optional shapes " + optShapes)
//  println("Optional shapes as CSV:\n" + writeCsv(optShapes))
}


