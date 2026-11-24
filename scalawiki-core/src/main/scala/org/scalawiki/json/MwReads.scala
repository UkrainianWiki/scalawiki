package org.scalawiki.json

import org.scalawiki.dto._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object MwReads {

  //  {"login":{"result":"NeedToken","token":"a504e9507bb8e8d7d3bf839ef096f8f7","cookieprefix":"ukwiki","sessionid":"37b1d67422436e253f5554de23ae0064"}}
  // {"login":{"result":"Success","lguserid":1,"lgusername":"ExampleBot","lgtoken":"8afaf1c733a4e667628be1f3ac176cdd","cookieprefix":"ukwiki","sessionid":"f4bf2533e14517401478383ca458feee"}}

  def loginResponseReads: Reads[LoginResponse] = (
    (__ \ "login" \ "result").read[String] and
      (__ \ "login" \ "token").readNullable[String] and
      (__ \ "login" \ "lgtoken").readNullable[String] and
      (__ \ "login" \ "lguserid").readNullable[Int] and
      (__ \ "login" \ "lgusername").readNullable[String] and
      (__ \ "login" \ "cookieprefix").readNullable[String] and
      (__ \ "login" \ "sessionid").readNullable[String] and
      (__ \ "login" \ "reason").readNullable[String]
  )(LoginResponse.apply _)

  def tokenReads: Reads[String] =
    (__ \ "query" \ "tokens" \ "csrftoken").read[String]

  def tokensReads: Reads[String] = (__ \ "tokens" \ "edittoken").read[String]

  def editResponseReads: Reads[String] = (__ \ "edit" \ "result").read[String]

  def uploadResponseReads: Reads[String] =
    (__ \ "upload" \ "result").read[String]

  def siteInfoReads: Reads[String] =
    (__ \ "query" \ "general" \ "generator").read[String]

  def editTokenReads: Reads[String] = (__ \\ "edittoken").read[String]

  def errorReads: Reads[MwException] = (
    (__ \ "error" \ "code").read[String] and
      (__ \ "error" \ "info").read[String]
  )((code, info) => MwException.apply(code, info))
}
