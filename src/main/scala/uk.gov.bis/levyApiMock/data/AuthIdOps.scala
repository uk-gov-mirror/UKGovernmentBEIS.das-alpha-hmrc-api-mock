package uk.gov.bis.levyApiMock.data

import scala.concurrent.{ExecutionContext, Future}

case class AuthId(scope: String, clientId: String, redirectUri: String, state: Option[String], id: Option[Long] = None, creationDate: Long = System.currentTimeMillis())

trait AuthIdOps {
  /**
    * @return a generated identifer for the authId record
    */
  def stash(authId: AuthId)(implicit ec: ExecutionContext): Future[Long]

  def pop(id: Long)(implicit ec: ExecutionContext): Future[Option[AuthId]]
}
