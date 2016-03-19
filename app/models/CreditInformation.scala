package models

import models.Numerals.IntergalacticNumerals

/**
  * Created by khn3193 on 3/20/16.
  */
case class CreditInformation(text : String) extends Input {
  override def isValid: Boolean = true

  override def process: Any = {
    if (!isValid) return "Invalid Input"
    val splitQuery = text.split(" ").filter(s => !IntergalacticNumerals.isValidSymbolName(s))

    val symbolString: List[String] = text.split(" ").filter(s => IntergalacticNumerals.isValidSymbolName(s)).toList
    val value = IntergalacticNumerals.valueOf(symbolString)
    val creditValue = splitQuery(2).toDouble / value

    IntergalacticNumerals.Credit(splitQuery(0), creditValue)
  }

}
