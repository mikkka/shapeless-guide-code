import shapeless._


object sum extends Poly2 {
  implicit val intIndCase: Case.Aux[Int, Int, Int] =
    at((x, y) => x + y)

  implicit val intStrCase: Case.Aux[Int, String, Int] =
    at((x, y) => x + y.length)
}

(10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum)
