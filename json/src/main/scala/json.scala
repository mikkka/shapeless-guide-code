import shapeless.{Coproduct, :+:, CNil, Inl, Inr, HNil, HList, ::, LabelledGeneric, Witness, Lazy}
import shapeless.labelled.FieldType

sealed abstract class Json
final case class JsonObject(fields: List[(String, Json)]) extends Json
final case class JsonArray(items: List[Json]) extends Json
final case class JsonString(value: String) extends Json
final case class JsonNumber(value: Double) extends Json
final case class JsonBoolean(value: Boolean) extends Json
case object JsonNull extends Json



object Json {
  def encode[A](value: A)(implicit encoder: JsonEncoder[A]): Json =
    encoder.encode(value)

  def stringify(json: Json): String = json match {
    case JsonObject(fields) => "{" + fields.map(stringifyField).mkString(",") + "}"
    case JsonArray(items)   => "[" + items.map(stringify).mkString(",") + "]"
    case JsonString(value)  => "\"" + escape(value) + "\""
    case JsonNumber(value)  => value.toString
    case JsonBoolean(value) => value.toString
    case JsonNull           => "null"
  }

  private def stringifyField(field: (String, Json)): String = {
    val (name, value) = field
    escape(name) + ":" + stringify(value)
  }

  private def escape(str: String): String =
    "\"" + str.replaceAll("\"", "\\\\\"") + "\""
}



trait JsonEncoder[A] {
  def encode(value: A): Json
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}



object JsonEncoder {
  def pure[A](func: A => Json): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): Json =
        func(value)
    }

  def pureObj[A](func: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject =
        func(value)
    }

  implicit val stringEnc: JsonEncoder[String] = pure(str => JsonString(str))

  implicit val intEnc: JsonEncoder[Int] = pure(num => JsonNumber(num))

  implicit val doubleEnc: JsonEncoder[Double] = pure(num => JsonNumber(num))

  implicit val booleanEnc: JsonEncoder[Boolean] = pure(bool => JsonBoolean(bool))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    pure(list => JsonArray(list.map(x => enc.encode(x))))

  implicit def optionEncder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    pure(opt => opt.map(x => enc.encode(x)).getOrElse(JsonNull))


   implicit val hnilEnc: JsonObjectEncoder[HNil] =
     pureObj(hnil => JsonObject(Nil))

  implicit def hlistEnc[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEnc: Lazy[JsonEncoder[H]],
    tEnc: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {

    val fieldName: String = witness.value.name
    pureObj {hlist =>
      val head = hEnc.value.encode(hlist.head)
      val tail = tEnc.encode(hlist.tail)

      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
    ): JsonEncoder[A] =

    pureObj { value =>
      hEncoder.value.encode(generic.to(value))
    }

  implicit val cnilEnc: JsonObjectEncoder[CNil] =
    pureObj(cnil => throw new Exception("can't touch it!"))

  implicit def coproductEnc[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEnc: Lazy[JsonEncoder[H]],
    tEnc: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    pureObj {
      case Inl(h) => JsonObject(List(typeName -> hEnc.value.encode(h)))
      case Inr(t) => tEnc.encode(t)
    }
  }

  implicit def genericEnc[A, H <: Coproduct](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]): JsonEncoder[A] = pureObj {value => hEncoder.value.encode(generic.to(value))}
}



final case class Employee(
  name    : String,
  number  : Int,
  manager : Boolean,
  eshape  : Shape
)

final case class IceCream(
  name        : String,
  numCherries : Int,
  inCone      : Boolean,
  maker       : Employee
)

sealed trait Shape

final case class Rectangle(
  width: Double,
  height: Double
) extends Shape

final case class Circle(
  radius: Double
) extends Shape



object Main extends Demo {

  val employee = Employee("Alice", 1, true, Circle(14.88))
//  val employee = Employee("Bob", 2, false)
//  val employee = Employee("Charlie", 3, false)

  val iceCream = IceCream("Cornetto", 0, true, Employee("Foo Barov", 23, false, Rectangle(42, 3)))
//  val iceCream = IceCream("Sundae", 1, false)

  val shape1: Shape = Rectangle(3, 4)
  val shape2: Shape = Circle(1)

  println(Json.stringify(Json.encode(iceCream)))
  println(Json.stringify(Json.encode(shape1)))
}


