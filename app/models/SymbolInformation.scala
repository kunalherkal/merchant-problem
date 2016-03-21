package models

import models.numerals.{IntergalacticNumerals, RomanNumerals}

/**
  * Created by khn3193 on 3/20/16.
  */
case class SymbolInformation(text : String) extends Input {

  override def isValid: Boolean = {
    val splitInfo = text.split(" ")
    val infoLength = splitInfo.length

    (infoLength == 3
      && splitInfo(1).toLowerCase == "is"
      && RomanNumerals.validSymbolName(splitInfo(2))
      && !IntergalacticNumerals.symbolPresent(splitInfo(0)))
  }

  override def process: Any = {
    if (!isValid) return "I have no idea what you are talking about"

    val splitQuery = text.split(" ")
    val romanSymbol = RomanNumerals.getSymbol(splitQuery(2))
    IntergalacticNumerals.Symbol(splitQuery(0), romanSymbol)

  }
}
