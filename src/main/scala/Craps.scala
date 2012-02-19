package com.yuvimasory.casinosim

class Craps() extends DiceGame() {

  override def name = "Craps"

  override def play(): CrapsState = {
    val comeOut = Craps roll()
    if (pointEstablished(comeOut)) {
      val point = 
      val buf = new scala.collection.mutable.ListBuffer[CrapsRoll]
      while (true) {
        val pointRoll = 
      }
    }
    else CrapsState(List(List(comeOut)))
  }

  private[this] def sum(crapsRoll: CrapsRoll): Int =
    crapsRoll._1 + crapsRoll._2

  private[this] def pointEstablished(crapsRoll: CrapsRoll): Boolean = {
    val tot = sum(crapsRoll)
    (tot == 7 || tot == 11 || tot == 3 || tot == 2 || tot == 12) == false
  }
}

object Craps {
  def roll(): CrapsRoll = (Die roll(), Die roll())
}

case class CrapsState(val rounds: List[CrapsRound]) extends GameState {

  override def summary(): String = ""
  override def ++(g: GameState): CrapsState = {
    val that = g.asInstanceOf[CrapsState]
    val nRounds = {
      that.rounds match {
        case Nil               => rounds
        case List(singleRound) => singleRound :: rounds
        case manyRounds        => rounds ++ manyRounds
      }
    }
    CrapsState(nRounds)
  }
}

object CrapsState {
  def empty: CrapsState = CrapsState(Nil)
}
