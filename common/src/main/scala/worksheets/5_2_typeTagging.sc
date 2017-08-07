trait Cherries

val nc = 42.asInstanceOf[Int with Cherries]
nc * 2
var foo: Int = nc

import shapeless.labelled.{FieldType, KeyTag}
import shapeless.syntax.singleton._
import shapeless.labelled.field
import shapeless.{HNil, Witness}

val someNumber = 123
val ger = "bar"

val numCherries = "cherries" ->> someNumber

val fCherries = field[Cherries](123)

def getFieldName[K, V](value: FieldType[K, V])
                      (implicit witness: Witness.Aux[K]): K = witness.value

def getFieldValue[K, V](value: FieldType[K, V]): V = value

getFieldName(numCherries)
getFieldValue(numCherries)

//getFieldName(fCherries)
getFieldValue(fCherries)


val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
