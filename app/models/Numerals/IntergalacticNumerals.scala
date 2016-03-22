package models.numerals

/**
  * Created by Kunal Herkal on 3/18/16.
  */
object IntergalacticNumerals {
  private var symbols : Set[Symbol] = Set.empty
  private var credits : Set[Credit] = Set.empty

  sealed case class Symbol(name : String, romanSymbol: RomanNumerals.Symbol)

  sealed case class Credit(name: String, value : Double)

  def addSymbol(symbol : IntergalacticNumerals.Symbol) : String = {
    if (!symbols.contains(symbol)) {
      symbols = symbols + symbol
      return "Added symbol: " + symbol.name
    }
    throw new IllegalArgumentException

  }

  def validSymbolCombo(symbols : List[String]) : Boolean = {
    val temp = symbols.map(symbol => getSymbol(symbol).romanSymbol)
    RomanNumerals.isValidCombo(temp)
  }

  private def getSymbol(symbolName: String) : Symbol = {
    symbols.find(s => s.name == symbolName).get
  }

  def valueOf(symbolCombo: List[String]) : Int = {
    val temp = symbolCombo.map(symbol => getSymbol(symbol).romanSymbol)
    RomanNumerals.valueOf(temp)
  }

  def symbolPresent(name : String) : Boolean = {
    symbols.map(s => s.name).contains(name)
  }

  def addCredit(credit : IntergalacticNumerals.Credit) : String = {
    if(!credits.contains(credit)) {
      credits = credits + credit
      return "Added credit: " + credit.name + " with value: " + credit.value
    }
    throw new IllegalArgumentException
  }

  def creditPresent(name: String) : Boolean = {
    credits.map(s => s.name).contains(name)
  }

  def getCredit(name: String) : Credit = {
    credits.find(s => s.name == name).get
  }

  def valueOfCredit(name : String, symbolValue: List[String]) : Double = {
    val credit = IntergalacticNumerals.getCredit(name)
    valueOf(symbolValue) * credit.value
  }
}
