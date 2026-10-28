package org.scalawiki.bots.finance

import org.scalawiki.dto.User

import scala.collection.immutable.SortedSet

class UserGroup(val name: String, val users: Set[User]) {

  override def toString = {
    val list =
      if (users.nonEmpty)
        s": ${SortedSet(users.map(_.login).toSeq: _*).mkString(", ")}"
      else ""
    s"${name.trim} — ${users.size}$list"
  }

}

object UserGroup {

  def empty(name: String) = new UserGroup(name, Set.empty[User])
}

class Board(val year: Int, users: Set[User]) extends UserGroup("Правління", users)
