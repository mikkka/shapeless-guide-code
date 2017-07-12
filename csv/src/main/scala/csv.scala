import java.util.Date

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}


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

  implicit def pairEncoder[A, B]
            (implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

  implicit val hnilEncoder: CsvEncoder[HNil] = pure(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList]
              (implicit  hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = {
    pure { case head :: tail =>
        hEncoder.encode(head) ++ tEncoder.encode(tail)
    }
  }

  implicit def genericEncoder[A, R]
              (implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A] = {
    pure {x =>
      enc.encode(gen.to(x))
    }
  }


  implicit val cnilEncoder: CsvEncoder[CNil] =
    pure(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct]
              (implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] = pure {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }
}

final case class Employee(name : String, number : Int, manager : Boolean)
final case class IceCream(name : String, numCherries : Int, inCone : Boolean)

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

sealed trait Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object MainCsv extends Demo {
  case class Booking(room: String, date: Date)

  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  def encodeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  println(encodeCsv("Dave"))
  println(encodeCsv(123))
  println(encodeCsv(true))
  println(encodeCsv(("Dave", true)))

  val reprEncoder = CsvEncoder[String :: Int :: Boolean :: HNil]
  println(encodeCsv("kek" :: 42 :: true :: HNil))

  implicit val genIce = Generic[IceCream]
  implicit val genEmp = Generic[Employee]
  implicit val genBook = Generic[Booking]
  implicit val shapeCoGen = Generic[Shape]

//  implicit val genBranch = Generic[Branch]

  println(encodeCsv(genIce.to(IceCream("imaice", 14, false))))
  println(encodeCsv(genEmp.to(Employee("imaemp", 88, true))))

  println(encodeCsv(IceCream("imaice", 14, false)))
  println(encodeCsv(Employee("imaemp", 88, true)))
  println(encodeCsv(List(Employee("imaemp", 88, true), Employee("omaemp", 89, false))))
  println(encodeCsv(List[Shape](Rectangle(14, 88), Circle(42))))

  //no cvs enc for date
  //println(encodeCsv(Booking("imaemp", new Date())))

  //CsvEncoder[Tree[Int]]

}


