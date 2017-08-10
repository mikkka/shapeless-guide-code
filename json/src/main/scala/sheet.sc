import shapeless.LabelledGeneric

val iceCream = IceCream("Cornetto", 2, true, Employee("Bogdan Titomir", 42, false, Circle(7)))
val gen = LabelledGeneric[IceCream].to(iceCream)


/*
::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("name")],String]
::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("numCherries")],Int]
::[Boolean with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("inCone")],Boolean]
HNil

= Cornetto :: 0 :: true :: HNil

*/


LabelledGeneric[Shape].to(Circle(1.0))

/*
shapeless.:+:[Circle with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("Circle")],Circle],
shapeless.:+:[Rectangle with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("Rectangle")],Rectangle],
shapeless.CNil]] = Inl(Circle(1.0))
 */

LabelledGeneric[Shape].to(Rectangle(14.0, 88.0))
/*
shapeless.:+:[Circle with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("Circle")],Circle],
shapeless.:+:[Rectangle with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("Rectangle")],Rectangle],
shapeless.CNil]] = Inr(Inl(Rectangle(14.0,88.0)))
 */
