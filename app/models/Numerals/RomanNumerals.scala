package models.Numerals

/**
  * Created by Kunal Herkal on 3/18/16.
  */
object RomanNumerals {
  sealed abstract class Symbol (val name : String,
                                val value : Int,
                                val canRepeat: Boolean,
                                val canBeSubtractedFrom: List[Symbol]) extends Ordered[Symbol] {

    def compare(that: Symbol) = this.value - that.value

    override def toString = s"Symbol(name=$name, value=$value)"
  }

  case object I extends Symbol("I", 1, true, List(V, X))
  case object V extends Symbol("V", 5, false, List.empty)
  case object X extends Symbol("X", 10, true, List(L, C))
  case object L extends Symbol("L", 50, false, List.empty)
  case object C extends Symbol("C", 100, true, List(D, M))
  case object D extends Symbol("D", 500, false, List.empty)
  case object M extends Symbol("M", 1000, true, List.empty)

  val symbols = Set(I, V, X, L, C, D, M)

  def getSymbol(symbolName : String) : Symbol = {
    if (!symbols.map(s => s.name).contains(symbolName))
      throw new IllegalArgumentException("Wrong symbol name encountered!: " + symbolName)

    symbols.find(s => s.name == symbolName).get
  }

  def validSymbolName(symbolName: String) : Boolean = {
    symbols.map(s => s.name).contains(symbolName)
  }

  def valueOf(symbols: List[Symbol]) : Int = {
    val temp = symbols.map(s => s.value)
    calculateValue(temp)
  }

  def isValidCombo(symbols : List[Symbol]) : Boolean = {
    val symbolRepetitionsTuple = symbolRepetitions(symbols)
    validOrder(symbolRepetitionsTuple) && validRepetitions(symbolRepetitionsTuple)
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

  def symbolRepetitions(symbols: List[RomanNumerals.Symbol]): List[(Symbol, Int)] = {
    var tupleList : List[(RomanNumerals.Symbol, Int)] = List.empty
    var tuple : (RomanNumerals.Symbol, Int) = (symbols.head, 0)
    symbols.foreach(s => {
      if(s.name == tuple._1.name) tuple = (s, tuple._2 + 1)
      else {
        tupleList = tupleList ++ List(tuple)
        tuple = (s, 1)
      }
    })
    tupleList = tupleList ++ List(tuple)
    tupleList
  }

  def validOrder(tupleList: List[(Symbol, Int)]) : Boolean = tupleList match {
    case head :: Nil => true
    case head :: tail if head._1.value > tail.head._1.value =>  true && validOrder(tail)
    case head :: tail if validSubtraction(head, tail.head) =>  true && validOrder(tail)
    case _ => false
  }

  def validSubtraction(current: (Symbol, Int), next: (RomanNumerals.Symbol, Int)): Boolean = {
    (current._1.value < next._1.value) && current._1.canBeSubtractedFrom.contains(next._1) && current._2 == 1
  }

  def validRepetitions(tupleList: List[(Symbol, Int)]): Boolean = {
    tupleList.map(tuple => {
      tuple match {
        case a if tuple._1.canRepeat && tuple._2 < 4 => true
        case b if !tuple._1.canRepeat && tuple._2 == 1 => true
        case _ => false
      }
    }).forall(a => a)
  }

}
