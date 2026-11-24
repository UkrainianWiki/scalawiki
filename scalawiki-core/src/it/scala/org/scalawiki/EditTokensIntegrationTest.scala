package org.scalawiki

class EditTokensIntegrationTest extends BaseIntegrationSpec {

  "test" should {
    "edit with csrf token" in {
      // TODO all tokens

      val bot = getUkWikiBot
      val result = login(bot)
      result === "Success"

      val response =
        await(bot.page(userSubpage("test")).edit("text1", Some("comment")))
      response === "Success"
    }
  }

}
