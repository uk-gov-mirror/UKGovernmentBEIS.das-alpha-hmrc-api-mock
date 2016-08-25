package uk.gov.bis.levyApiMock.data

import scala.concurrent.{ExecutionContext, Future}

case class Application(name: String, applicationID: String, clientID: String, clientSecret: String, serverToken: String)

trait ApplicationOps {
  def validate(id: String, secret: Option[String], grantType: String)(implicit ec: ExecutionContext): Future[Boolean]
}