package ex

import util.Optionals.Optional
import util.Sequences.*

trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

case class ItemImpl(code: Int, name: String, tags: Sequence[String]) extends Item

object Item:
  // def apply(code: Int, name: String, tags: Sequence[String] = Sequence.empty): Item = ItemImpl(code, name, tags)
  def apply(code: Int, name: String, tags: String*): Item = ItemImpl(code, name, Sequence(tags: _*))

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
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

class WarehouseImpl extends Warehouse:
  private var items: Sequence[Item] = Sequence.empty

  override def store(item: Item): Unit =
    items = items.concat(Sequence(item))

  override def contains(itemCode: Int): Boolean =
//    items.contains(items.filter(_.code == itemCode).head.orElse(null))
    items.contains(items.find(_.code == itemCode).orElse(null))

  override def searchItems(tag: String): Sequence[Item] =
    items.filter(_.tags.contains(tag))

  override def retrieve(code: Int): Optional[Item] =
    items.find(_.code == code)

  override def remove(item: Item): Unit =
    items = items.filter(_ != item)


object Warehouse:
  def apply(): Warehouse = new WarehouseImpl

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

//  val dellXps = Item(33, "Dell XPS 15", Sequence("notebook"))
//  val dellInspiron = Item(34, "Dell Inspiron 13", Sequence("notebook"))
//  val xiaomiMoped = Item(35, "Xiaomi S1", Sequence("moped", "mobility"))
  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  // warehouse.contains(dellXps.code) // false
  assert(!warehouse.contains(dellXps.code), "Test 1 failed")
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  assert(warehouse.contains(dellXps.code), "Test 2 failed")
  // warehouse.contains(dellXps.code) // true
  assert(warehouse.contains(dellXps.code), "Test 3 failed")
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  assert(warehouse.contains(dellInspiron.code) && warehouse.contains(xiaomiMoped.code), "Test 4 failed")
  // warehouse.searchItems("mobility") // Sequence(xiaomiMoped)
  assert(warehouse.searchItems("mobility") == Sequence(xiaomiMoped), "Test 5 failed")
  // warehouse.searchItems("notebook") // Sequence(dellXps, dell Inspiron)
  assert(warehouse.searchItems("notebook") == Sequence(dellXps, dellInspiron), "Test 6 failed")
  // warehouse.retrieve(11) // None
  assert(warehouse.retrieve(11).isEmpty, "Test 7 failed")
  // warehouse.retrieve(dellXps.code) // Just(dellXps)
  assert(warehouse.retrieve(dellXps.code) == Optional.Just(dellXps), "Test 8 failed")
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  // warehouse.retrieve(dellXps.code) // None
  assert(warehouse.retrieve(dellXps.code).isEmpty, "Test 9 failed")

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private Sequence of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/