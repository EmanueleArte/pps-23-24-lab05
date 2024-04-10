package ex

import Warehouse.*
import util.Optionals.Optional
import util.Sequences.*

def OptionalToScalaOptional[A](opt: Optional[A]): Option[A] = opt match
  case Optional.Just(v) => Some(v)
  case Optional.Empty() => None

object SameTag:
  def unapply(items: Sequence[Item]): Option[String] =
    val allTags = items.flatMap(_.tags)
    OptionalToScalaOptional(allTags.find(tag => allTags.filter(_ == tag).size == items.size))

@main def main(): Unit =
  val itemsCommonTag = Sequence(
    Item(1, "item1", "tech"),
    Item(2, "item2", "tech", "clothes"),
    Item(3, "item3", "sports", "tech")
  )

  SameTag.unapply(itemsCommonTag) match
    case Some(tag) => println(s"$itemsCommonTag have same tag: $tag")
    case None => println(s"$itemsCommonTag have different tags")

  val itemsDifferentTag = Sequence(
    Item(1, "item1", "tech"),
    Item(2, "item2", "clothes"),
    Item(3, "item3", "sports")
  )

  SameTag.unapply(itemsDifferentTag) match
    case Some(tag) => println(s"$itemsDifferentTag have same tag: $tag")
    case None => println(s"$itemsDifferentTag have different tags")