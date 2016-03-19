package models

import models.Numerals.{IntergalacticNumerals, RomanNumerals}

/**
  * Created by khn3193 on 3/20/16.
  */
case class SymbolInformation(text : String) extends Input {

  override def isValid: Boolean = {
    true
  }

  override def process: Any = {
    if (!isValid) return "Invalid Input"

    val splitQuery = text.split(" ")
    val romanSymbol = RomanNumerals.getSymbol(splitQuery(2))
    IntergalacticNumerals.Symbol(splitQuery(0), romanSymbol)

  }
}
