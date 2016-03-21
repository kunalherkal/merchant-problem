package app.models

import models._
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by Kunal Herkal on 3/21/16.
  */
class InputTest extends FlatSpec with Matchers {
  "Get" should " get CreditQuery Input" in {
    val input = Input.get("how many Credits is glob prok Silver ?")
    assert(input.isInstanceOf[CreditQuery])
  }

  "Get" should " get CreditInformation Input" in {
    val input = Input.get("glob glob Silver is 34 Credits")
    assert(input.isInstanceOf[CreditInformation])
  }

  "Get" should " get SymbolQuery Input" in {
    val input = Input.get("how much is pish tegj glob glob ?")
    assert(input.isInstanceOf[SymbolQuery])
  }

  "Get" should " get SymbolInformation Input" in {
    val input = Input.get("tegj is L")
    assert(input.isInstanceOf[SymbolInformation])
  }

}
