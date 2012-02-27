package com.yuvimasory

package object casinosim {
  val CommaFmt = new java.text.DecimalFormat("###,###,###,###")
  val DecFmt = new java.text.DecimalFormat("###.####")
  implicit def dub2perc(d: Double) = new ToPerc(d)
  class ToPerc(d: Double) {
    def percent = DecFmt format (d * 100)
    def pretty = DecFmt format d
  }
  implicit def i2comma(i: Int) = new ToComma(i)
  class ToComma(i: Int) {
    def comma = CommaFmt format i
  }
}
