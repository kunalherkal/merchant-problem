package controllers

import javax.inject._
import models.Numerals.{IntergalacticNumerals, RomanNumerals}
import play.api._
import play.api.mvc._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action {
    info("glob is I")
    info("prok is V")
    info("pish is X")
    info("tegj is L")

    println(RomanNumerals.valueOf("XXIV"))
    println(RomanNumerals.valueOf("XLII"))
    println(RomanNumerals.valueOf("MCMXLIV"))

    println(IntergalacticNumerals.valueOf("pish tegj glob glob"))

    Ok(views.html.index("Your new application is ready."))
  }

  def info(query: String): Int = {
    val splitQuery = query.split(" ")
    val romanSymbol = RomanNumerals.getSymbol(splitQuery(2))

    IntergalacticNumerals.addSymbol(splitQuery(0), romanSymbol)
    IntergalacticNumerals.symbols.foreach(println)
    0
  }

  def value(query: String): Int = {
    0
  }



}
