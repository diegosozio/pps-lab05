package ex

import util.Optionals
import util.Optionals.*
import util.Optionals.isEmpty
import util.Optionals.Optional.*
import util.Sequences.*

case class ItemImpl(code: Int, name: String, tags: Sequence[String]) extends Item

trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  def apply(code: Int, name: String, tags: String*): Item = ItemImpl(code, name, Sequence(tags: _*)) 

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse:
  def apply(): Warehouse = WarehouseImpl()

  class WarehouseImpl extends Warehouse:
    private var items: Sequence[Item] = Sequence.empty

    // Metodo per memorizzare un oggetto
    def store(item: Item): Unit =
      items = items.concat(Sequence(item)) // Usare concat per aggiungere l'oggetto

    // Metodo per controllare se un oggetto esiste
    def contains(itemCode: Int): Boolean =
      !items.find(_.code == itemCode).isEmpty

    // Metodo per cercare oggetti per tag
    def searchItems(tag: String): Sequence[Item] =
      items.filter(_.tags.contains(tag)) // Usa filter per cercare per tag

    // Metodo per recuperare un oggetto per codice
    def retrieve(code: Int): Optional[Item] =
      items.find(_.code == code) // Trova l'oggetto e restituisci Optional

    // Metodo per rimuovere un oggetto per riferimento
    def remove(item: Item): Unit =
      items = items.filterNot(_.code == item.code) // Rimuovi l'oggetto

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook") // Creare oggetti Item
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  assert(!warehouse.contains(dellXps.code)) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  assert(warehouse.contains(dellXps.code)) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  assert(warehouse.searchItems("mobility") == Sequence(xiaomiMoped)) // Sequence(xiaomiMoped)
  assert(warehouse.searchItems("notebook") == Sequence(dellXps, dellInspiron)) // Sequence(dellXps, dell Inspiron)
  assert(warehouse.retrieve(11) == Optional.Empty) // None
  assert(warehouse.retrieve(dellXps.code) == Just(dellXps)) // Just(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  assert(warehouse.retrieve(dellXps.code) == Optional.Empty) // None