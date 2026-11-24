package org.scalawiki.mockserver

class LoginMockServerSpec extends BaseMockServerSpec {

  val (user, absentUser) = ("userName", "absentUser")
  val (password, wrongPassword) = ("secretPassword", "wrongPassword")

  def withCredentials(user: String, password: String) =
    Map(
      "action" -> "login",
      "format" -> "json",
      "lgname" -> user,
      "lgpassword" -> password
    )

  val needToken =
    """{ "login": {
                "result": "NeedToken",
                "token": "token-value+\\",
                "cookieprefix":"enwiki",
                "sessionid":"sessionid-value"
                }
           }"""

  val loginSuccess =
    """{ "login": {
                "result": "Success",
                "lguserid":1,
                "lgusername":"ExampleBot",
                "lgtoken":"token-value",
                "cookieprefix":"enwiki",
                "sessionid":"sessionid-value"
                }
           }"""

  def result(code: String) = s"""{"login":{"result":"$code"}}"""

  val wrongPassResult = result("WrongPass")
  val notExistsResult = result("NotExists")
  val throttledResult = result("Throttled")

  "login" should {
    "succesfully login ExampleBot" in {
      val loginAction = withCredentials(user, password)
      stubOk(loginAction ++ Map("lgtoken" -> "token-value+\\"), loginSuccess)
      stubOk(loginAction, needToken)

      val result = login(getBot, user, password)
      result === "Success"
    }
  }

  "login" should {
    "reject login ExampleBot with wrong passwd" in {
      val loginAction = withCredentials(user, wrongPassword)
      stubOk(loginAction ++ Map("lgtoken" -> "token-value+\\"), wrongPassResult)
      stubOk(loginAction, needToken)

      val result = login(getBot, user, wrongPassword)
      result === "WrongPass"
    }
  }

  "login" should {
    "reject login of wrong user" in {
      val loginAction = withCredentials(absentUser, wrongPassword)
      stubOk(loginAction ++ Map("lgtoken" -> "token-value+\\"), notExistsResult)
      stubOk(loginAction, needToken)
      val result = login(getBot, absentUser, wrongPassword)
      result === "NotExists"
    }
  }

}
