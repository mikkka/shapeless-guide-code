import shapeless._

object myPoly extends Poly1 {
  implicit val intCase: Case.Aux[Int, Double] =
    at(num => num / 2.0)

  implicit val stringCase: Case.Aux[String, Int] =
    at(str => str.length)
}

myPoly(42)
myPoly("kekus")

object multiply extends Poly2 {
  implicit val intIntCase: Case.Aux[Int, Int, Int] =
    at((a, b) => a * b)
  implicit val intStrCase: Case.Aux[Int, String, String] =
    at((a, b) => b.toString * a)
}

multiply(3, 4)
multiply(3, "kekus")


import scala.math.Numeric

object total extends Poly1 {
  implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] =
    at(x => num.toDouble(x))

  implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] =
    at(opt => opt.map(num.toDouble).getOrElse(0.0))

  implicit def list2Double[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] =
    at(list => num.toDouble(list.sum))

/*
  //this will fail implicit resolution inside poly
  implicit def list2Int[A](implicit num: Numeric[A]): Case.Aux[List[A], Int] =
    at(list => num.toInt(list.sum))
*/
}

total(10)
// res15: Double = 10.0
total(Option(20.0))
// res16: Double = 20.0
total(List(1L, 2L, 3L))
