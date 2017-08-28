import shapeless._

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object sizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int] =
    at(identity)

  implicit val stringCase: Case.Aux[String, Int] =
    at(_.length)

  implicit val booleanCase: Case.Aux[Boolean, Int] =
    at(bool => if(bool) 1 else 0)
}

object doubleAll extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int] =
    at(_ * 2)

  implicit val stringCase: Case.Aux[String, String] =
    at(x => x + x)

  implicit val booleanCase: Case.Aux[Boolean, Boolean] =
    at(bool => !bool)
}


(10 :: "hello" :: true :: HNil).map(sizeOf)

Generic[IceCream].to(IceCream("ball", 2, true)).map(doubleAll)



object valueAndSizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
    at(x => x :: x :: HNil)

  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
    at(x => x :: x.length :: HNil)

  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
    at(x => x :: (if(x) 1 else 0) :: HNil)
}

Generic[IceCream].to(IceCream("ball", 2, true)).flatMap(valueAndSizeOf)

