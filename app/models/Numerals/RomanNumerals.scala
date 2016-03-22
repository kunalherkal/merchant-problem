package models.numerals

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

  private val symbols = Set(I, V, X, L, C, D, M)

  def getSymbol(symbolName : String) : Symbol = {
    if (!symbols.map(s => s.name).contains(symbolName))
      throw new IllegalArgumentException("Wrong roman symbol name encountered!: " + symbolName)

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

    def loop(value: Int, symbolValues : List[Int]) : Int =  symbolValues match {
      case a :: Nil => a + value
      case head :: tail if head >= tail.head =>  loop(value + head, tail)
      case head :: tail =>  loop(value - head, tail)
      case Nil => throw new IllegalArgumentException("Cannot find value from empty list of symbols")
    }

    loop(0, symbolValues)
  }

  def symbolRepetitions(symbols: List[Symbol]): List[(Symbol, Int)] = {
    packDuplicatesAndCount(symbols)
  }

  def packDuplicatesAndCount(list : List[Symbol]): List[(Symbol,Int)] = {

    list match{
      case Nil => Nil
      case x :: tail =>{
        val value = list.takeWhile{s => s == x}
        (value.head, value.length) :: packDuplicatesAndCount(list.drop(value.length))
      }
    }
  }

  def validOrder(tupleList: List[(Symbol, Int)]) : Boolean = tupleList match {
    case head :: Nil => true
    case head :: tail if head._1.value > tail.head._1.value =>  validOrder(tail)
    case head :: tail if validSubtraction(head, tail.head) =>  validOrder(tail)
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
