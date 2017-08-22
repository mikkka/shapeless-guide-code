import shapeless._
import shapeless.record._

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val sundae = LabelledGeneric[IceCream].to(IceCream("Sundae", 1, false))


/*
sundae:
shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("name")],String],
shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("numCherries")],Int],
shapeless.::[Boolean with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("inCone")],Boolean],
shapeless.HNil]]] = Sundae :: 1 :: false :: HNil
*/

sundae.get('name)

sundae.updated('name, "foobar")

sundae.updateWith('name)("MASSIVE " + _)
