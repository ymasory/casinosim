package com.yuvimasory.casinosim

import java.io.{ BufferedReader, File, FileReader }

import scala.annotation.tailrec
import scala.collection.{ mutable => m }

sealed trait Game {

  /* interface to implement */
  type MyWager <: Wager
  type MyGameRound <: GameRound
  type MyPlayer <: GamePlayer

  val Name: String
  val RoundKey: Option[String] = None
  val player: MyPlayer

  trait GameRound {
    def serialize: String
    def outcomes: List[Pair[Wager, Int]]
  }

  trait Wager {
    val Name: String
  }

  trait GamePlayer {
    def play(): MyGameRound
  }

  case class OutcomeCounter(val counter: Counter[Wager, Int] = new Counter[Wager, Int]()) {

    def :+(res: MyGameRound): OutcomeCounter = {
      val foldRes = res.outcomes.foldLeft(counter) { (acc, pair) =>
        val Pair(wager, outcome) = pair
        acc increment (wager, outcome)
      }
      new OutcomeCounter(foldRes)
    }

    def summary = {
      val buf = new StringBuffer
      val wagers = { counter.map.keys.map { _._1 } }.toSet
      for (wager <- wagers) {
        val total = counter sumOf wager
        buf append {
          "### %s over %s trials ###%n" format (wager.Name, CommaFmt format total)
        }
        for (((pair @ Pair(w, outcome), count)) <- counter.map if wager == w) {
          buf append {
            "%s had outcome %s a total of %s times%n" format (
              wager.Name, outcome, CommaFmt format count
            )
          }
          val prob = (counter prob (wager, outcome)) * 100
          buf append {
            "Probability(%s) = %s%%%n" format (outcome, DecFmt format prob)
          }
        }
        buf append { "%n" format() }
      }
      buf.toString
    }
  }
}

abstract class CardGame(val deckDesc: DeckDescription) extends Game {
  protected[this] def createShoe(): Shoe = deckDesc match {
    case FiniteDecks(n) => FiniteShoe next n
    case InfiniteDecks  => InfiniteShoe next()
  }
}

trait DiceGame extends Game
trait CoinGame extends Game

case class Counter[A, B](val map: Map[Pair[A, B], Int] = Map.empty[Pair[A, B], Int]) {

  def increment(a: A, b: B) = count(a, b, 1)
  def count(a: A, b: B, count: Int): Counter[A, B] = {
    val pair = (a, b)
    val nCount: Int = map.get(pair) match {
      case None           => count
      case Some(oldCount) => count + oldCount
    }
    val nMap = map + (pair -> nCount)
    new Counter(nMap)
  }

  def prob(a: A, b: B) = {
    val denom = sumOf(a).toDouble
    val numer = {
      map.get((a, b)) match {
        case None => 0
        case Some(count) => count
      }
    }.toDouble
    numer/denom
  }

  def sumOf(a: A) = map.foldLeft(0){ (acc, pair) =>
    val Pair(k @ Pair(aIn, outcome), count) = pair
    acc + { if (a == aIn) map(k) else 0 }
  }
}
