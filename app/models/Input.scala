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
  def get(text: String): Input = {
    if (text.contains("?")) SymbolQuery(text) else SymbolInformation(text)
  }
}

