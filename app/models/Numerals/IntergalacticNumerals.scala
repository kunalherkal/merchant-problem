package models.Numerals

/**
  * Created by Kunal Herkal on 3/18/16.
  */
object IntergalacticNumerals {
  val symbols : scala.collection.mutable.Set[Symbol] = scala.collection.mutable.Set.empty

  sealed case class Symbol(name : String, romanSymbol: RomanNumerals.Symbol) {

  }

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

  def getSymbol(symbolName: String) : Symbol = {
    symbols.find(s => s.name == symbolName).get
  }

  def valueOf(symbolCombo : String) : Int = {
    val temp = symbolCombo.split(" ").map(symbol => getSymbol(symbol).romanSymbol).toList
    RomanNumerals.valueOf(temp)
  }

  def valueOf(symbolCombo: List[String]) : Int = {
    val temp = symbolCombo.map(symbol => getSymbol(symbol).romanSymbol)
    RomanNumerals.valueOf(temp)
  }

  def isValidSymbolName(name : String) : Boolean = {
    symbols.map(s => s.name).contains(name)
  }
}
