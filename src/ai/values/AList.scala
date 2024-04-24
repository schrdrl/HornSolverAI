
package ai.values

//AList
sealed trait AList extends AVal
case object ANil extends AList { def refs = List() }
case class ACons(hd: ARef, tl: ARef) extends AList { def refs = List(hd, tl) }
case class AMany(elems: ARef) extends AList { def refs = List(elems) }