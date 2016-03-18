package models.Numerals

/**
  * Created by Kunal Herkal on 3/18/16.
  */
object RomanNumerals {
  sealed abstract class Symbol (val name : String, val value : Int) extends Ordered[Symbol] {

    def compare(that: Symbol) = this.value - that.value

    override def toString = s"Symbol(name=$name, value=$value)"
  }

  case object I extends Symbol("I", 1)
  case object V extends Symbol("V", 5)
  case object X extends Symbol("X", 10)
  case object L extends Symbol("L", 50)
  case object C extends Symbol("C", 100)
  case object D extends Symbol("D", 500)
  case object M extends Symbol("M", 1000)

  val symbols = Set(I, V, X, L, C, D, M)

  def getSymbol(symbolName : String) : Symbol = {
    if (!symbols.map(s => s.name).contains(symbolName))
      throw new IllegalArgumentException("Wrong symbol name encountered!: " + symbolName)

    symbols.find(s => s.name == symbolName).get
  }

  def valueOf(symbols: String) : Int = {
    val temp = symbols.map(s => getSymbol(s.toString)).toList.map(s => s.value)
    calculateValue(temp)
  }

  def valueOf(symbols: List[Symbol]) : Int = {
    val temp = symbols.map(s => s.value)
    calculateValue(temp)
  }

  private def calculateValue(symbolValues : List[Int]) : Int = {

    def loop(symbolValues : List[Int]) : Int =  symbolValues match {
      case a :: Nil => a
      case head :: tail if head >= tail.head =>  head + loop(tail)
      case head :: tail =>  - head + loop(tail)
      case Nil => throw new IllegalArgumentException("Can not find value from empty list of symbols")
    }

    loop(symbolValues)
  }

}
