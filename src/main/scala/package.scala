package com.yuvimasory

package object casinosim {
  val rand = new java.security.SecureRandom
  val decFmt = new java.text.DecimalFormat("###.###")
  val commaFmt = new java.text.DecimalFormat("###,###,###,###")

  type CrapsRound = List[CrapsRoll]
  type CrapsRoll = Pair[Byte, Byte]
}
