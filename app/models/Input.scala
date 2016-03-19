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
    if (text.contains("?")){
      if(text.contains("credit")) CreditQuery(inputText) else SymbolQuery(inputText)
    }  else {
      if(text.contains("credit")) CreditInformation(inputText) else SymbolInformation(inputText)
    }
  }
}

