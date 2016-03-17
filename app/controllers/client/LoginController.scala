package controllers.client

import javax.inject.Inject

import actions.client.ClientUserAction
import db.client.{SchemeClaimDAO, DASUserDAO, SchemeDAO}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}

import scala.concurrent.{ExecutionContext, Future}

case class UserData(name: String, password: String)

class LoginController @Inject()(schemeDAO: SchemeDAO, dasUserDAO: DASUserDAO, UserAction: ClientUserAction, schemeClaimDAO: SchemeClaimDAO)(implicit exec: ExecutionContext) extends Controller {

  val userForm = Form(
    mapping(
      "username" -> text,
      "password" -> text
    )(UserData.apply)(UserData.unapply)
  )
  
  def showLogin = Action {
    Ok(views.html.client.login(userForm))
  }

  def handleLogin = Action.async { implicit request =>
    userForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(views.html.client.login(formWithErrors))),
      userData => {
        dasUserDAO.validate(userData.name, userData.password).map {
          case Some(user) => Redirect(controllers.routes.ClientController.index()).withSession(("userId", user.id.toString))
          case None => Ok(views.html.client.login(userForm.withError("username", "Bad user name or password")))
        }
      }
    )
  }

  def logout = Action {
    Redirect(controllers.client.routes.LoginController.showLogin()).withNewSession
  }
}