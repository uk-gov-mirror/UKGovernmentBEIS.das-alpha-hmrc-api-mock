package controllers.api

import javax.inject._

import actions.api.ApiAction
import db.levy.{LevyDeclarationDAO, SchemeDAO}
import models.{EnglishFraction, LevyDeclaration, LevyDeclarations, PayrollMonth}
import org.joda.time.LocalDate
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.hmrc.domain.EmpRef

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class EpayeController @Inject()(schemeDAO: SchemeDAO, levyDeclarationDAO: LevyDeclarationDAO, ApiAction: ApiAction)(implicit exec: ExecutionContext) extends Controller {
  def levyDeclarations(empref: EmpRef) = ApiAction.async { implicit request =>
    if (request.emprefs.contains(empref)) {
      levyDeclarationDAO.byEmpref(empref.value).map { ds =>
        val decls = ds.map { d =>
          LevyDeclaration(PayrollMonth(d.year, d.month), d.amount)
        }

        val englishFraction = EnglishFraction(0.83, new LocalDate)

        Ok(Json.toJson(LevyDeclarations(empref, englishFraction, 15000, decls)))
      }
    } else {
      Future.successful(Unauthorized)
    }
  }

  def root = Action.async { _ => ??? }
}
