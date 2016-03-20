package models

import models.Numerals.IntergalacticNumerals

/**
  * Created by khn3193 on 3/20/16.
  */
case class CreditInformation(text : String) extends Input {

  override def isValid: Boolean = {
    val splitInfo = text.split(" ")
    val symbolList = splitInfo.filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    val remainingQuery = splitInfo.filter(s => !IntergalacticNumerals.symbolPresent(s))

    println(symbolList.size)
    symbolList.foreach(s => "CredInfo: " + println(s))
    remainingQuery.foreach(println)
    println(IntergalacticNumerals.validSymbolCombo(symbolList))

    (splitInfo.size > 4
      && remainingQuery.length == 4
      && symbolList.size == splitInfo.length - 4
      && !IntergalacticNumerals.creditPresent(remainingQuery(0))
      && remainingQuery(1).toLowerCase == "is"
      && remainingQuery(3).toLowerCase == "credits"
      && IntergalacticNumerals.validSymbolCombo(symbolList))
  }

  override def process: Any = {
    if (!isValid) return "I have no idea what you are talking about"
    val splitQuery = text.split(" ").filter(s => !IntergalacticNumerals.symbolPresent(s))

    val symbolString: List[String] = text.split(" ").filter(s => IntergalacticNumerals.symbolPresent(s)).toList
    val value = IntergalacticNumerals.valueOf(symbolString)
    val creditValue = splitQuery(2).toDouble / value

    IntergalacticNumerals.Credit(splitQuery(0), creditValue)
  }

}
