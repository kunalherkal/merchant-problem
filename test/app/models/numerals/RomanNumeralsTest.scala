package app.models.numerals

import models.numerals.RomanNumerals
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by Kunal Herkal on 3/21/16.
  */
class RomanNumeralsTest extends FlatSpec with Matchers {

  "SymbolRepetitions" should "return list of tuples containing (Symbol, repetition count)" in {
    val symbolList = List(RomanNumerals.X, RomanNumerals.X, RomanNumerals.X, RomanNumerals.V, RomanNumerals.I, RomanNumerals.I)

    val response = RomanNumerals.symbolRepetitions(symbolList).toArray
    val tuple1 = response(0)
    val tuple2 = response(1)
    val tuple3 = response(2)

    assert(tuple1._1 == RomanNumerals.X)
    assert(tuple1._2 == 3)

    assert(tuple2._1 == RomanNumerals.V)
    assert(tuple2._2 == 1)

    assert(tuple3._1 == RomanNumerals.I)
    assert(tuple3._2 == 2)
  }

  "SymbolRepetitions" should "return Nil" in {
    val symbolList = List.empty[RomanNumerals.Symbol]

    assert(symbolList == Nil)

  }

}
