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

  case class OutcomeCounter(val counter: Counter = new Counter()) {

    def :+(res: MyGameRound): OutcomeCounter = {
      val foldRes = res.outcomes.foldLeft(counter) { (acc, pair) =>
        val Pair(wager, outcome) = pair
        acc increment (wager, outcome)
      }
      new OutcomeCounter(foldRes)
    }

    def summary = {
      val buf = new StringBuffer
      val wagers = counter.wagers
      for (wager <- wagers) {
        val total = counter total wager
        buf append {
          "# %s over %s trials #%n" format (wager.Name, CommaFmt format total)
        }
        buf append { "## outcome / total / probability ##%n" format() }
        val outcomeCounts = counter outcomeCounts wager
        for ((outcome, count) <- outcomeCounts) {
          val prob = (counter prob (wager, outcome)) * 100
          buf append {
            "%s / %s / %s%%%n" format (
              outcome,
              CommaFmt format count,
              DecFmt format prob
            )
          }
        }
        buf append { "## key figures ##%n" format() }
        val ev = counter expectedValue wager
        buf append { "Expected Value = %s%n" format (DecFmt format ev) }
        val edge = (ev * 100) * -1
        buf append { "House Advantage = %s%%%n" format (DecFmt format edge) }
        val payout = 100 - edge
        buf append { "Payout = %s%%%n" format (DecFmt format payout) }
        val variance = counter variance wager
        buf append { "Variance = %s%%%n" format (DecFmt format variance) }
        buf append { "%n" format() }
      }
      buf.toString
    }
  }

  case class Counter(private val map: Map[Pair[Wager, Int], Int] = Map.empty) {

    def wagers: List[Wager] = {
      map.keys.map { _._1 }
    }.toList.distinct.sortWith(_.Name < _.Name)

    def outcomeCounts(wager: Wager): List[(Int, Int)] = {
      map.keys.toList.flatMap { pair =>
        val Pair(w, outcome) = pair
        val count = map(pair)
        if (w == wager) Some((outcome, count)) else None
      }
    }.sortWith(_._1 < _._1)

    def increment(wager: Wager, outcome: Int) = count(wager, outcome, 1)

    def count(wager: Wager, outcome: Int, count: Int): Counter = {
      val pair = (wager, outcome)
      val nCount: Int = map.get(pair) match {
        case None           => count
        case Some(oldCount) => count + oldCount
      }
      val nMap = map + (pair -> nCount)
      new Counter(nMap)
    }

    def expectedValue(wager: Wager) = {
      def outcomeCounts = this outcomeCounts wager
      outcomeCounts.foldLeft(0D) { (acc, pair) =>
        val Pair(outcome, counts) = pair
        val probOutcome = prob(wager, outcome)
        acc + (probOutcome * outcome)
      }
    }

    def prob(wager: Wager, outcome: Int) = {
      val denom = total(wager).toDouble
      val numer = {
        map.get((wager, outcome)) match {
          case None => 0
          case Some(count) => count
        }
      }.toDouble
      numer/denom
    }

    def total(wager: Wager) = map.foldLeft(0){ (acc, pair) =>
      val Pair(k @ Pair(aIn, outcome), count) = pair
      acc + { if (wager == aIn) map(k) else 0 }
    }

    def variance(wager: Wager) = 1D
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

