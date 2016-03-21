package models

/**
  * Created by khn3193 on 3/19/16.
  */
abstract class Input {
  val text: String

  def isValid : Boolean

  def process : Any

}

object Input {
  def get(inputText: String): Input = {
    val text = inputText.toLowerCase

    text match {
      case a if a.contains("?") && a.contains("credit") => CreditQuery(inputText)
      case b if b.contains("?") => SymbolQuery(inputText)
      case c if c.contains("credit") => CreditInformation(inputText)
      case _ => SymbolInformation(inputText)
    }

  }
}

