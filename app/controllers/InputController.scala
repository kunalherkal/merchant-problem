package controllers

import com.google.inject.{Inject, Singleton}
import models.Input
import models.Numerals.IntergalacticNumerals
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{MessagesApi, I18nSupport}
import play.api.mvc.{AnyContent, Action, Controller}

/**
  * Created by khn3193 on 3/19/16.
  */
@Singleton
class InputController @Inject() (val messagesApi: MessagesApi) extends Controller with I18nSupport {

  val inputForm: Form[InputForm] = Form {
    mapping(
      "Enter your query" -> nonEmptyText
    )(InputForm.apply)(InputForm.unapply)
  }

  def sample(response: String) = Action {
    Ok(views.html.index(inputForm, response))
  }

  def submitInput: Action[AnyContent] = Action { implicit request =>

    inputForm.bindFromRequest.fold(
      errorForm => {
        Ok(views.html.index(errorForm, "Failed"))
      },
      InputForm => {
        println("Got Input query: " + InputForm)
        val input = Input.get(InputForm.text)
        println(input)
        val outcome = input.process
println(outcome)
        val response: String = outcome match {
          case a : Int => a.toString
          case e : Double => e.toString
          case b : String => b
          case c : IntergalacticNumerals.Symbol => IntergalacticNumerals.addSymbol(c)
          case d : IntergalacticNumerals.Credit => IntergalacticNumerals.addCredit(d)
        }
        Redirect(routes.InputController.sample(response))
      }
    )

  }

}

case class InputForm(text : String)