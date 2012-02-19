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

object CrapsAnalyzer {
  import java.io.BufferedReader

  private[this] val Pat = """\d/\d""".r

  def analyze(file: java.io.File) = {
    val reader = new java.io.BufferedReader(new java.io.FileReader(file))
    val stats = this loop reader

    val passWins = stats.passWins.toDouble
    val passLoses = stats.passLoses.toDouble
    val dontWins = stats.dontWins.toDouble
    val dontPushes = stats.dontPushes.toDouble

    val total = passWins + passLoses
    println("TOTAL ROLLS: " + (commaFmt format total))
    println()

    println("PASS BET")
    val passWinPercent = passWins/total * 100
    val passLossPercent = passLoses/total * 100
    val passHouseEdge = passLossPercent - passWinPercent
    println("pass wins %s%%" format passWinPercent)
    println("pass loses %s%%" format passLossPercent)
    println("pass house advantage: %s%%" format passHouseEdge) 

    println()
    println("DON'T PASS BET")
    val dontWinPercent = dontWins/total * 100
    val dontPushPercent = dontPushes/total * 100
    val dontLossPercent = passWinPercent
    val dontHouseEdge = dontLossPercent - dontWinPercent
    println("don't pass wins %s%%" format dontWinPercent)
    println("don't pass loses %s%%" format dontLossPercent)
    println("don't pass pushes %s%%" format dontPushPercent)
    println("don't pass house advantage: %s%%" format dontHouseEdge) 
  }

  private[this] def loop(in: BufferedReader, stats: Stats = Stats.empty): Stats = {
     Option(in readLine()) match {
       case Some(line) => {
         val skip = line startsWith "#"
         if (skip) loop(in, stats)
         else {
           val rollStrs: Vector[String] = Vector.empty ++ (Pat findAllIn line)
           val rolls: List[CrapsRoll] = rollStrs.map { s =>
             val c1 = s(0).toString
             val c2 = s(2).toString
             val b1 = c1.toByte
             val b2 = c2.toByte
             CrapsRoll(b1, b2)
           }.toList
           val res = ShooterResult fromRolls rolls
           loop(in, stats :+ res)
         }
       }
       case None => stats
     }
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

  case class Stats(
    val passWins: Int,
    val passLoses: Int,
    val dontWins: Int,
    val dontPushes: Int
  ) {
    def :+(res: ShooterResult): Stats = {
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
      Stats(
        passWins = passWins + { if (pw) 1 else 0 },
        passLoses = passLoses + { if (pw) 0 else 1 },
        dontWins = dontWins  + { if (dw) 1 else 0 },
        dontPushes = dontPushes + { if (dp) 1 else 0 }
      )
    }
  }

  object Stats {
    val empty = Stats(0, 0, 0, 0)
  }

  sealed trait ShooterResult
  sealed trait ShooterWin extends ShooterResult
  case object ComeOutWin extends ShooterWin
  case object PointWin extends ShooterWin

  sealed trait ShooterLoss extends ShooterResult
  case object SevenOut extends ShooterLoss
  sealed trait CrapOut extends ShooterLoss
  case object CrapOut23 extends CrapOut
  case object CrapOut12 extends CrapOut
}
