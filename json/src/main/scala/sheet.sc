import shapeless.LabelledGeneric

val iceCream = IceCream("Cornetto", 0, true)
val gen = LabelledGeneric[IceCream].to(iceCream)
