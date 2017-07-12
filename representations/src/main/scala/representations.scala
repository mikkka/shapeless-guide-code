import shapeless.{:+:, ::, CNil, Generic, HNil, Inl, Inr}

final case class Employee(
  name    : String,
  number  : Int,
  manager : Boolean
)

final case class IceCream(
  name        : String,
  numCherries : Int,
  inCone      : Boolean
)

final case class Red(r: Int)
final case class Amber(a: String)
final case class Green(g: Double)

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

object Main extends Demo {
  val product: String :: Int :: Boolean :: HNil =
    "Sunday" :: 1 :: false :: HNil


  println(product.head)
  println(product.tail.head)

  42L :: product

  val employee = Employee("Bill", 1, true)
  val iceCream = IceCream("Cornetto", 0, true)

  val iceCreamGen = Generic[IceCream]
  val employeeGen = Generic[Employee]

  iceCreamGen.to(IceCream("plombir", 22, true))
  iceCreamGen.from("Sunday" :: 1 :: false :: HNil)

  employeeGen.from(iceCreamGen.to(IceCream("plombir", 22, true)))

  type Light = Red :+: Amber :+: Green :+: CNil
  val red: Light = Inl(Red(42))
  val amb: Light = Inr(Inl(Amber("foo")))
  val grn: Light = Inr(Inr(Inl(Green(88.0))))


  def lightStr(light: Light): String = light match {
    case Inl(Red(r))             => (r * 2).toString
    case Inr(Inl(Amber(a)))      => a
    case Inr(Inr(Inl(Green(g)))) => (g / 2).toString
    case Inr(Inr(Inr(inr)))      => inr.toString
  }


  lightStr(red)
  lightStr(amb)
  lightStr(grn)

    val coGen = Generic[Shape]

  println(coGen)

  println(coGen.to(Rectangle(3.0, 4.0)))
  println(coGen.to(Circle(1.0)))
}
