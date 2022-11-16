package uk.gov.bis.levyApiMock.models

import play.api.libs.json.{JsObject, JsString, _}

import scala.util.{Failure, Success, Try}

class SimpleObjectWrites[T](val valueGetter: T => String) extends Writes[T] {
  override def writes(value: T): JsValue = JsString(valueGetter(value))
}

class SimpleObjectReads[T](val fieldName: String, val constructor: String => T) extends Reads[T] {
  override def reads(js: JsValue): JsResult[T] = Try {
    js match {

      case v: JsString => v.validate[String].map(constructor)
      case v: JsObject => (v \ fieldName).validate[String].map(constructor)
      case noParsed    => JsError(s"Could not read Json value of $fieldName in $noParsed")
    }
  } match {
    case Success(jsResult)  => jsResult
    case Failure(exception) => JsError(exception.getMessage())
  }
}
