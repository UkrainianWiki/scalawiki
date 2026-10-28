package org.scalawiki.dto.cmd.query.list

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.query.Module

/** https://www.mediawiki.org/wiki/API:Users
  *
  * @param params
  */

case class Users(override val params: UsParam*)
    extends Module[ListArg](
      "us",
      "users",
      "Get information about a list of users."
    )
    with ListArg
    with ArgWithParams[UsParam, ListArg]

trait UsParam extends Parameter[Any]

case class UsUsers(override val args: Iterable[String])
    extends StringListParameter(
      "ususers",
      "A list of user names to get information for."
    )
    with UsParam

/** ?action=query&amp;list=users&amp;usprop=
  */
case class UsProp(override val args: UsPropArg*)
    extends EnumParameter[UsPropArg](
      "usprop",
      "Which properties to get (Default: none)."
    )
    with UsParam

/** Trait for usprop= arguments
  */

trait UsPropArg extends EnumArg[UsPropArg] {
  val param = UsProp
}

/** usprop= arguments
  */

object UsBlockInfo
    extends EnumArgument[UsPropArg](
      "blockinfo",
      "Whether the user is blocked, by whom and why"
    )
    with UsPropArg

object UsGroups
    extends EnumArgument[UsPropArg]("groups", "All groups the user belongs to")
    with UsPropArg

object UsImplicitGroups
    extends EnumArgument[UsPropArg](
      "implicitgroups",
      "All groups a user is automatically a member of"
    )
    with UsPropArg

object UsRights extends EnumArgument[UsPropArg]("rights", "All rights the user has") with UsPropArg

object UsEditCount
    extends EnumArgument[UsPropArg](
      "editcount",
      "The number of edits the user has made"
    )
    with UsPropArg

object UsRegistration
    extends EnumArgument[UsPropArg](
      "registration",
      "The time and date the user registered at"
    )
    with UsPropArg

object UsEmailable
    extends EnumArgument[UsPropArg](
      "emailable",
      "Whether the user can and wants to receive e-mail through Special:Emailuser"
    )
    with UsPropArg

object UsGender
    extends EnumArgument[UsPropArg](
      "gender",
      "Tags the gender of the user. Returns \"male\", \"female\", or \"unknown\""
    )
    with UsPropArg
