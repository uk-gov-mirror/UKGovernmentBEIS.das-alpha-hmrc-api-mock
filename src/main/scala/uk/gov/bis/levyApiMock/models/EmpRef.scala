package uk.gov.bis.levyApiMock.models

import java.net.{URLDecoder, URLEncoder}

import play.api.libs.json._

case class EmpRef(taxOfficeNumber: String, taxOfficeReference: String) extends TaxIdentifier {
  override def toString = value

  def value = taxOfficeNumber + "/" + taxOfficeReference

  def encodedValue = URLEncoder.encode(value, "UTF-8")
}

object EmpRef extends ((String, String) => EmpRef) {

  implicit val empRefWrite: Writes[EmpRef] = new SimpleObjectWrites[EmpRef](_.value)
  implicit val empRefRead: Reads[EmpRef] = new Reads[EmpRef] {
    override def reads(js: JsValue): JsResult[EmpRef] = js match {
      case v: JsString => v.validate[String].map(EmpRef.fromIdentifiers)
      case v: JsObject => for {
        ton <- (v \ "taxOfficeNumber").validate[String]
        tor <- (v \ "taxOfficeReference").validate[String]
      } yield EmpRef(ton, tor)
      case noParsed => throw new Exception(s"Could not read Json value of empRef in $noParsed")
    }
  }

  def fromIdentifiers(slashSeparatedIdentifiers: String): EmpRef = {
    val empRefPattern = """([^/]*)/([^/]*)""".r
    URLDecoder.decode(slashSeparatedIdentifiers, "UTF-8") match {
      case empRefPattern(first, second) => EmpRef(first, second)
      case _                            => throw new IllegalArgumentException("EmpRef requires two identifiers separated by a slash")
    }
  }

  def decode(encodedEmpRef: String) = URLDecoder.decode(encodedEmpRef, "UTF-8")
}
