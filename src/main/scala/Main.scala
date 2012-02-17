package com.yuvimasory.cardgames

/* Main */
object Main {

  val Iters = 100000000

  def main(args: Array[String]) = war()

  def war() {
    val sim = WarSim next 1
    sim start()
    sim ! Iters
  }
}

