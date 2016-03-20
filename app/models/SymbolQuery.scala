package models

import models.Numerals.IntergalacticNumerals

/**
  * Created by khn3193 on 3/20/16.
  */
case class SymbolQuery(text : String) extends Input {

  override def isValid: Boolean = {
    val splitQuery = text.filter(s => s!= '?').split(" ")
    val symbolList = splitQuery.filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    (splitQuery.length > 3
      && splitQuery(0) == "how"
      && splitQuery(1) == "much"
      && splitQuery(2) == "is"
      && symbolList.size == splitQuery.length - 3
      && IntergalacticNumerals.validSymbolCombo(symbolList))
  }

  override def process: Any = {
    if (!isValid) return "Invalid Input"
    val symbolString: List[String] = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    IntergalacticNumerals.valueOf(symbolString)
  }
}
