package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences

import scala.util.Random
import util.Sequences.*
import Sequence.*

import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters

case class Pair[A, B](a: A, b: B)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private val random = Random()
  private var selected = empty[Pair[Int, Int]]
  private val gridSize = size * size
  private val minesSet = Sequence((for _ <- 0 until mines yield generateMine()): _*)

  @tailrec
  private def generateMine(): Pair[Int, Int] = Pair(random.nextInt(size), random.nextInt(size)) match
    case pair if minesSet.contains(pair) => generateMine()
    case pair => pair

  private def neighbours(x: Int, y: Int): Int =
    (for
      xx <- (x - 1) to (x + 1)
      yy <- (y - 1) to (y + 1)
      pair = Pair(xx, yy)
      if minesSet.contains(pair)
    yield pair).size

  private def size(seq: Sequence[Pair[Int, Int]]): Int = seq match
    case Cons(_, tail) => 1 + size(tail)
    case empty => 0

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if minesSet.contains(Pair(x, y)) then
      OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
    else
      selected = selected.concat(Sequence(Pair(x, y)))
      OptionToOptional(ScalaOptional.Just(neighbours(x, y)))

  def won: Boolean =
    size(selected) + size(minesSet) == gridSize
