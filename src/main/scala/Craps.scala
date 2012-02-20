package com.yuvimasory.casinosim

class Craps() extends DiceGame() {

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

  case class CrapsRound(val rolls: List[CrapsRoll]) extends GameRound {
    override def repr = rolls.map(_.repr).mkString(" ")
  }
}

case class CrapsRoll(val b1: Byte, val b2: Byte) {
  def repr = "%s/%s" format (b1, b2)
  lazy val sum = b1 + b2
  def comeoutResult = sum match {
    case 7 | 11     => Winner
    case 2 | 3 | 12 => CrapOut
    case p          => PointSet(p)
  }
  def craps = sum match {
    case 3 | 2 | 12 => true
    case _          => false
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

object Shooter {
  def shoot(): CrapsRoll = CrapsRoll(Die roll(), Die roll())
}

object CrapsAnalyzer extends RoundsAnalyzer {

  override type MyStats = CrapsStats
  override type MyResult = ShooterResult
  override val emptyStats = CrapsStats(0, 0, 0, 0)

  private[this] val Pat = """\d/\d""".r

  override def parseLine(line: String): ShooterResult = {
    val rollStrs: Vector[String] = Vector.empty ++ (Pat findAllIn line)
    val rolls: List[CrapsRoll] = rollStrs.map { s =>
      val c1 = s(0).toString
      val c2 = s(2).toString
      val b1 = c1.toByte
      val b2 = c2.toByte
      CrapsRoll(b1, b2)
    }.toList
    ShooterResult fromRolls rolls
  }

  object ShooterResult {
    def fromRolls(lst: List[CrapsRoll]): ShooterResult = {
      lst match {      
        case List(comeout) => comeout.sum match {
          case 2 | 3 => CrapOut23
          case 12    => CrapOut12
          case _     => ComeOutWin
        }
        case lst => {
          val last = lst.last
          if (last.sum == 7) SevenOut else PointWin
        }
      }
    }
  }

  case class CrapsStats(
    val passWins: Int,
    val passLoses: Int,
    val dontWins: Int,
    val dontPushes: Int
  ) extends AggregateStats {

    def :+(res: ShooterResult): CrapsStats = {
      val (pw, dw, dp) = res match {
        case _: ShooterWin        => (true, false, false)
        case loss: ShooterLoss    => {
          val dw =
            loss match {
              case SevenOut | CrapOut23 => true
              case CrapOut12            => false
            }
          (false, dw, !dw)
        }
      }
      CrapsStats(
        passWins = passWins + { if (pw) 1 else 0 },
        passLoses = passLoses + { if (pw) 0 else 1 },
        dontWins = dontWins  + { if (dw) 1 else 0 },
        dontPushes = dontPushes + { if (dp) 1 else 0 }
      )
    }

    override def summary = {
      val buf = new StringBuffer
      def append(s: String = "") = buf append { "%s%n" format s }

      val passWins = this.passWins.toDouble
      val passLoses = this.passLoses.toDouble
      val dontWins = this.dontWins.toDouble
      val dontPushes = this.dontPushes.toDouble

      val total = passWins + passLoses
      append("TOTAL ROLLS: " + (CommaFmt format total))
      append()

      append("PASS BET")
      val passWinPercent = passWins/total * 100
      val passLossPercent = passLoses/total * 100
      val passHouseEdge = passLossPercent - passWinPercent
      append("pass wins %s%%" format (DecFmt format passWinPercent))
      append("pass loses %s%%" format (DecFmt format passLossPercent))
      append {
        "pass house advantage: %s%%" format (DecFmt format passHouseEdge)
      }

      append()
      append("DON'T PASS BET")
      val dontWinPercent = dontWins/total * 100
      val dontPushPercent = dontPushes/total * 100
      val dontLossPercent = passWinPercent
      val dontHouseEdge = dontLossPercent - dontWinPercent
      append("don't pass wins %s%%" format (DecFmt format dontWinPercent))
      append("don't pass loses %s%%" format (DecFmt format dontLossPercent))
      append("don't pass pushes %s%%" format (DecFmt format dontPushPercent))
      append {
        "don't pass house advantage: %s%%".format {
          DecFmt format dontHouseEdge
        }
      }
      buf.toString
    }
  }

  sealed trait ShooterResult extends RoundResult
  sealed trait ShooterWin extends ShooterResult
  case object ComeOutWin extends ShooterWin
  case object PointWin extends ShooterWin

  sealed trait ShooterLoss extends ShooterResult
  case object SevenOut extends ShooterLoss
  sealed trait CrapOut extends ShooterLoss
  case object CrapOut23 extends CrapOut
  case object CrapOut12 extends CrapOut
}
