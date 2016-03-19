package models

import models.Numerals.IntergalacticNumerals

/**
  * Created by khn3193 on 3/20/16.
  */
case class CreditQuery(text : String) extends Input {
  override def isValid: Boolean = true

  override def process: Any = {
    if (!isValid) return "Invalid Input"

    val symbolString: List[String] = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.isValidSymbolName(s)).toList
    val creditName = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.isValidCreditName(s))(0)

    IntergalacticNumerals.valueOfCredit(creditName, symbolString)
  }
}
