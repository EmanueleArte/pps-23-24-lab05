package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences

import scala.util.Random
import util.Sequences.*
import Sequence.*

import scala.jdk.javaapi.OptionConverters

case class Pair[A, B](a: A, b: B)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private val random = Random()
  private val selected = empty[Pair[Int, Int]]
  private val gridSize = size * size
  private val minesSet = Sequence[Pair[Int, Int]]((for _ <- 0 until mines yield generateMine()): _*)

  private def generateMine(): Pair[Int, Int] = Pair(random.nextInt(size), random.nextInt(size)) match
    case pair if minesSet.contains(pair) => generateMine()
    case pair => pair

  println(s"mines: $minesSet")

  def hit(x: Int, y: Int): java.util.Optional[Integer] =

    OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter

  def won = false
