package org.scalawiki

trait LoginInfo {
  def login: String

  def password: String
}

case class LoginInfoValue(login: String, password: String) extends LoginInfo

object LoginInfo {

  /** Set to `true` (the build sets it for tests) to make [[fromEnv]] ignore the
    * environment: no bot logs in as `SCALAWIKI_LOGIN` then. A test that must log in
    * needs `-Dscalawiki.allowTestLogin=true` on sbt's command line, which no CI job sets.
    */
  val AnonymousProperty = "scalawiki.anonymous"

  def fromEnv(
      loginProp: String = "SCALAWIKI_LOGIN",
      passwordProp: String = "SCALAWIKI_PASSWORD"
  ): Option[LoginInfo] =
    if (sys.props.get(AnonymousProperty).contains("true")) None
    else
      for (
        login <- sys.env.get(loginProp).filter(_.nonEmpty);
        password <- sys.env.get(passwordProp).filter(_.nonEmpty)
      )
        yield LoginInfoValue(login, password)

}
