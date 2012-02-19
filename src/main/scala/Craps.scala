package com.yuvimasory.casinosim

class Craps() extends DiceGame[CrapsRound]() {

  override val name = "Craps"

  override def play(): CrapsRound = {
    val roll = Shooter shoot()
    CrapsRound {
      roll :: {
        roll.comeoutResult match {
          case p: PointSet          => p resolvePoint()
          case _: DecisiveResult    => Nil
        }
      }
    }
  }
}

case class CrapsRoll(val b1: Byte, val b2: Byte) {
  lazy val repr = "%s/%s" format (b1, b2)
  lazy val sum = b1 + b2
  def comeoutResult = sum match {
    case 7 | 11     => Winner
    case 2 | 3 | 12 => CrapOut
    case p          => PointSet(p)
  }
}

sealed trait ComeoutResult
sealed trait DecisiveResult extends ComeoutResult
case object Winner extends DecisiveResult
case object CrapOut extends DecisiveResult
case class PointSet(p: Int) extends ComeoutResult {
  def resolvePoint(past: List[CrapsRoll] = Nil): List[CrapsRoll] = {
    val roll = Shooter shoot()
    val allRolls = roll :: past
    roll.sum match {
      case `p` | 7 => allRolls.reverse
      case n       => this resolvePoint allRolls
    }
  }
}

case class CrapsRound(val rolls: List[CrapsRoll]) extends GameRound {
  override val repr = rolls.map(_.repr).mkString(" ")
}

object Shooter {
  def shoot(): CrapsRoll = CrapsRoll(Die roll(), Die roll())
}
