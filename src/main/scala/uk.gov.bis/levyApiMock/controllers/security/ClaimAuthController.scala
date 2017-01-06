package uk.gov.bis.levyApiMock.controllers.security

import javax.inject.{Inject, Singleton}

import cats.data.EitherT
import cats.instances.future._
import play.api.mvc.{Action, Controller}
import uk.gov.bis.levyApiMock.data._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ClaimAuthController @Inject()(scopes: ScopeOps, authIds: AuthRequestOps, clients: ClientOps)(implicit ec: ExecutionContext) extends Controller {

  implicit class ErrorSyntax[A](ao: Option[A]) {
    def orError(err: String): Either[String, A] = ao.fold[Either[String, A]](Left(err))(a => Right(a))
  }

  /**
    * Handle the initial oAuth request
    */
  def authorize(scopeName: String, clientId: String, redirectUri: String, state: Option[String]) = Action.async {
    implicit request =>
      val authIdOrError = for {
        _ <- EitherT(clients.forId(clientId).map(_.orError("unknown client id")))
        _ <- EitherT(scopes.byName(scopeName).map(_.orError("unknown scope")))
      } yield AuthRequest(scopeName, clientId, redirectUri, state)


      authIdOrError.value.flatMap {
        case Left(err) => Future.successful(BadRequest(err))
        case Right(a) => authIds.stash(a).map(id => Redirect(routes.GrantScopeController.show(id)))
      }
  }
}