package models

import models.numerals.IntergalacticNumerals

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
      && splitQuery(0).toLowerCase == "how"
      && splitQuery(1).toLowerCase == "many"
      && splitQuery(2).toLowerCase == "credits"
      && splitQuery(3).toLowerCase == "is"
      && IntergalacticNumerals.creditPresent(restQuery(4))
      && IntergalacticNumerals.validSymbolCombo(symbolCombo))

  }

  override def process: Any = {
    if (!isValid) return "I have no idea what you are talking about"

    val symbolString: List[String] = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    val creditName = text.filter(s => s != '?').split(" ").filter(s => IntergalacticNumerals.creditPresent(s))(0)

    val query = symbolString mkString(" ")
    query + " " + creditName + " is " + IntergalacticNumerals.valueOfCredit(creditName, symbolString) + " Credits"
  }
}
