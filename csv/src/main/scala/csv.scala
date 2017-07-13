import shapeless.Generic
import shapeless.{HList, ::, HNil}
import shapeless.{Coproduct, :+:, CNil, Inl, Inr}


trait CsvEncoder[A] {
  def width: Int
  def encode(value: A): List[String]
}



object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc

  def pure[A](w: Int)(func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      val width = w
      def encode(value: A): List[String] =
        func(value)
    }

   implicit val stringEnc: CsvEncoder[String] =
     pure(1)(str => List(str))

   implicit val intEnc: CsvEncoder[Int] =
     pure(1)(num => List(num.toString))

  implicit val doubleEnc: CsvEncoder[Double] =
    pure(1)(num => List(num.toString))

   implicit val booleanEnc: CsvEncoder[Boolean] =
     pure(1)(bool => List(if(bool) "yes" else "no"))

  implicit def optEncoder[T](implicit enc: CsvEncoder[T]) : CsvEncoder[Option[T]] =
    pure(enc.width) {
      case Some(x) => enc.encode(x)
      case None    => List.fill(enc.width)("")
    }

  implicit val hnilEncoder: CsvEncoder[HNil] = pure(0)(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList]
              (implicit  hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = {
    pure(hEncoder.width + tEncoder.width) { case head :: tail =>
        hEncoder.encode(head) ++ tEncoder.encode(tail)
    }
  }

  implicit val cnilEncoder: CsvEncoder[CNil] =
    pure(0)(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct]
              (implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] =
    pure(hEncoder.width + tEncoder.width) {
      case Inl(h) => hEncoder.encode(h) ++ List.fill(tEncoder.width)("")
      case Inr(t) => List.fill(hEncoder.width)("") ++ tEncoder.encode(t)
    }


  implicit def genericEncoder[A, R]
              (implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A] = {
    pure(enc.width) {x =>
      enc.encode(gen.to(x))
    }
  }
}

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape
final case class Huyangle(width: Double, height: Double, olo: String) extends Shape

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
      Circle(6),
      Huyangle(14, 88, "olo")
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
  println("Optional shapes " + optShapes)
  println("Optional shapes as CSV:\n" + writeCsv(optShapes))
}


