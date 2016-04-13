package actions.api

import com.google.inject.Inject
import data.levy.GatewayIdSchemeOps
import data.oauth2.AuthRecordOps
import play.api.mvc.Results._
import play.api.mvc.{ActionBuilder, _}

import scala.concurrent.{ExecutionContext, Future}

class AuthorizedRequest[A](val request: Request[A], val emprefs: List[String]) extends WrappedRequest[A](request)

class AuthorizedActionBuilder(taxId: String, scope: String, authRecords: AuthRecordOps, enrolments: GatewayIdSchemeOps)(implicit ec: ExecutionContext)
  extends ActionBuilder[Request] {
  override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    val BearerToken = "Bearer (.+)".r

    request.headers.get("Authorization") match {
      case Some(BearerToken(accessToken)) => validateToken(accessToken, taxId, scope).flatMap {
        case true => block(request)
        case false => unauthorized("Bearer token does not grant access to the requested resource")
      }
      case Some(h) => unauthorized("Authorization header should be a Bearer token")
      case None => unauthorized("No Authorization header found")
    }
  }

  def validateToken[A](accessToken: String, taxId: String, scope: String): Future[Boolean] =
    authRecords.find(accessToken, taxId, scope).map(_.isDefined)

  private def unauthorized(message: String): Future[Result] = Future.successful(Unauthorized(message))
}


class AuthorizedAction @Inject()(authRecords: AuthRecordOps, enrolments: GatewayIdSchemeOps)(implicit ec: ExecutionContext) {
  def apply[A](taxId: String, scope: String): AuthorizedActionBuilder =
    new AuthorizedActionBuilder(taxId, scope, authRecords, enrolments)(ec)
}