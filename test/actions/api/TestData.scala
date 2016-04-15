package actions.api

import data.levy.GatewayIdSchemeOps
import data.oauth2.{AuthRecord, AuthRecordOps}

import scala.concurrent.{ExecutionContext, Future}

object TestData {
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  lazy val testAction = new AuthorizedAction(authRecords, enrolments)

  val validToken = "abc"

  val invalidToken = "xyz"

  val testEmprefs = List("123/AB12345")


  val authRecords = new AuthRecordOps {
    override def all()(implicit ec: ExecutionContext): Future[Seq[AuthRecord]] = Future.successful(Seq())

    override def clearExpired()(implicit ec: ExecutionContext): Future[Unit] = Future.successful(())

    override def find(accessToken: String)(implicit ec: ExecutionContext): Future[Option[AuthRecord]] = Future.successful {
      if (accessToken == validToken) Some(AuthRecord(accessToken, "read:test", "gateway1", "client1", System.currentTimeMillis() + 1000, System.currentTimeMillis() - 1000))
      else None
    }

    override def create(token: AuthRecord)(implicit ec: ExecutionContext): Future[Unit] = Future.successful(())

    override def find(accessToken: String, taxId: String, scope: String)(implicit ec: ExecutionContext): Future[Option[AuthRecord]] = find(accessToken)

    override def expire(token: String)(implicit ec: ExecutionContext): Future[Int] = Future.successful(1)
  }

  val enrolments = new GatewayIdSchemeOps {
    override def bindEmprefs(gatewayId: String, emprefs: List[String])(implicit ec: ExecutionContext): Future[Unit] = Future.successful(())

    override def emprefsForId(gatewayId: String)(implicit ec: ExecutionContext): Future[Seq[String]] = Future.successful {
      if (gatewayId == "gateway1") testEmprefs else Seq()
    }
  }

}
