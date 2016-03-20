package models.Numerals

/**
  * Created by Kunal Herkal on 3/18/16.
  */
object IntergalacticNumerals {
  val symbols : scala.collection.mutable.Set[Symbol] = scala.collection.mutable.Set.empty
  val credits : scala.collection.mutable.Set[Credit] = scala.collection.mutable.Set.empty

  sealed case class Symbol(name : String, romanSymbol: RomanNumerals.Symbol)

  sealed case class Credit(name: String, value : Double)

  def addSymbol(symbolName: String, romanSymbol : RomanNumerals.Symbol) : String = {
    val symbol = Symbol(symbolName, romanSymbol)
    if(!symbols.contains(symbol))
      symbols.add(symbol)
    "Added symbol: " + symbolName
  }

  def addSymbol(symbol : IntergalacticNumerals.Symbol) : String = {
    if(!symbols.contains(symbol))
      symbols.add(symbol)
    "Added symbol: " + symbol.name
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
    if(!credits.contains(credit))
      credits.add(credit)
    "Added credit: " + credit.name + " with value: " + credit.value
  }

  def isValidCreditName(name: String) : Boolean = {
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
