package ex

import util.Optionals.Optional
import util.Optionals.Optional.*
import util.Sequences.*

/**
 * sameTag Extractor Object
 * This object extracts the common tag from a sequence of items.
 */
object sameTag:
  def unapply(items: Sequence[Item]): Option[String] =
    convertOptionalToOption(
      items.head.flatMap(firstItem =>
        val firstItemTags = firstItem.tags
        firstItemTags.find(tag => items.forall(_.tags.contains(tag)))
      )
    )

  // Funzione helper per convertire da Optional a Option
  private def convertOptionalToOption[A](opt: Optional[A]): Option[A] = opt match
    case Just(a) => Some(a)
    case Empty() => None

@main def mainSameTagExtractor(): Unit =
  val warehouses = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  warehouses.store(dellXps)
  warehouses.store(dellInspiron)
  warehouses.store(xiaomiMoped)

  val items = Sequence(dellXps, dellInspiron, xiaomiMoped)

  items match
    case sameTag(tag) => println(s"$items have the same tag: $tag")
    case _ => println(s"$items have different tags")