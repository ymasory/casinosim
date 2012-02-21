package com.yuvimasory.casinosim

import scala.annotation.tailrec

class Craps() extends DiceGame() {

  override type MyWager = CrapsWager
  override type MyGameRound = CrapsRound
  override type MyStats = CrapsStats
  override type MyPlayer = CrapsPlayer

  override val Name = "Craps"
  override val EmptyStats = new CrapsStats()

  override val player: CrapsPlayer = new CrapsPlayer()

  private[this] val Pat = """\d/\d""".r

  class CrapsRound(rolls: List[CrapsRoll]) extends GameRound {

    private[Craps] def res: CrapsResult = rolls match {
      case List(comeout) => comeout.sum match {
        case 2 | 3  => CrapOut23
        case 12     => CrapOut12
        case 7 | 11 => ComeoutWin
        case _      => sys.error("impossible single-round roll: " + comeout)
      }
      case _ => rolls.last.sum match {
        case 7 => SevenOut
        case _ => PointWin
      }
    }
    override def serialize = rolls.map(_.serialize).mkString(" ")
    override def outcomes = List(Pass, DontPass).map {
      wager => (wager, res outcomeOf wager)
    }
  }

  sealed trait CrapsWager extends Wager
  case object Pass extends CrapsWager {
    override val Name = "Pass"
  }
  case object DontPass extends CrapsWager {
    override val Name = "Don't Pass"
  }

  class CrapsPlayer() extends GamePlayer() {

    override def play(): CrapsRound = {
      val comeoutRoll = Shooter shoot()
      val pointRolls = {
        comeoutRoll.sum match {
          case 2 | 3 | 7 | 11 | 12 => Nil
          case p      => {
            @tailrec
            def resolvePoint(past: List[CrapsRoll] = Nil): List[CrapsRoll] = {
              val roll = Shooter shoot()
              val rollSum = roll.sum
              val pointRolls = roll :: past
              rollSum match {
                case 7 | `p` => pointRolls.reverse
                case _       => resolvePoint(pointRolls)
              }
            }
            resolvePoint()
          }
        }
      }
      new CrapsRound(comeoutRoll :: pointRolls)
    }
  }

  /*
  override def deserializeRound(line: String): CrapsRound = {
    val rollStrs: Vector[String] = Vector.empty ++ (Pat findAllIn line)
    val rolls: List[CrapsRoll] = rollStrs.map { s =>
      val c1 = s(0).toString
      val c2 = s(2).toString
      val b1 = c1.toByte
      val b2 = c2.toByte
      CrapsRoll(b1, b2)
    }.toList
    new CrapsRound(rolls)
  }
  */

  class CrapsStats() extends AggregateStats {
    override def :+(res: CrapsRound): CrapsStats = EmptyStats
    override def summary = "summary"
  }

  /* private */
  private[this] object Shooter {
    def shoot(): CrapsRoll = CrapsRoll(Die roll(), Die roll())
  }

  /* internal */
  private[this] sealed trait CrapsResult {
    def outcomeOf(w: CrapsWager): Int
  }
  private sealed trait ShooterWin extends CrapsResult {
    override def outcomeOf(w: CrapsWager) = w match {
      case Pass     => 1
      case DontPass => -1
    }
  }
  private[this] case object ComeoutWin extends ShooterWin
  private[this] case object PointWin extends ShooterWin
  private[this] sealed trait ShooterLoss extends CrapsResult
  private[this] case object SevenOut extends ShooterLoss {
    override def outcomeOf(w: CrapsWager) = w match {
      case Pass     => -1
      case DontPass => 1
    }
  }
  private[this] sealed trait CrapOut extends ShooterLoss
  private[this] case object CrapOut23 extends CrapOut {
    override def outcomeOf(w: CrapsWager) = w match {
      case Pass     => -1
      case DontPass => 1
    }
  }
  private[this] case object CrapOut12 extends CrapOut {
    override def outcomeOf(w: CrapsWager) = w match {
      case Pass     => -1
      case DontPass => 0
    }
  }
  private[this] case class CrapsRoll(val b1: Byte, val b2: Byte) {
    def serialize = "%s/%s" format (b1, b2)
    lazy val sum = b1 + b2
  }
}
