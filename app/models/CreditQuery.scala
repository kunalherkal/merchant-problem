package models

import models.Numerals.IntergalacticNumerals

/**
  * Created by khn3193 on 3/20/16.
  */
case class CreditQuery(text : String) extends Input {

  override def isValid: Boolean = {
    val splitQuery = text.filter(s => s != '?').split(" ")
    val symbolCombo = splitQuery.filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    val restQuery = splitQuery.filter(s => !IntergalacticNumerals.symbolPresent(s))

    (splitQuery.length > 5
      && restQuery.length == 5
      && splitQuery(0) == "how"
      && splitQuery(1) == "many"
      && splitQuery(2) == "Credits"
      && splitQuery(3) == "is"
      && IntergalacticNumerals.isValidCreditName(restQuery(4))
      && IntergalacticNumerals.validSymbolCombo(symbolCombo))

  }

  override def process: Any = {
    if (!isValid) return "Invalid Input"

    val symbolString: List[String] = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    val creditName = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.isValidCreditName(s))(0)

    IntergalacticNumerals.valueOfCredit(creditName, symbolString)
  }
}
