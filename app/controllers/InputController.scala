package controllers

import com.google.inject.{Inject, Singleton}
import models.Input
import models.numerals.IntergalacticNumerals
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
      "input" -> nonEmptyText
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
        val input = Input.get(InputForm.text)
        val outcome: Any = input.process
        val response: String = outcome match {
          case a : String => a
          case b : IntergalacticNumerals.Symbol => IntergalacticNumerals.addSymbol(b)
          case c : IntergalacticNumerals.Credit => IntergalacticNumerals.addCredit(c)
        }
        Redirect(routes.InputController.sample(response))
      }
    )

  }

}

