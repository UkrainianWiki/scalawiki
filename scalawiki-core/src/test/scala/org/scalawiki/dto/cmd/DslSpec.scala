package org.scalawiki.dto.cmd

import org.scalawiki.dto.cmd.query.list.{CmTitle, CategoryMembers, EiTitle, EmbeddedIn}
import org.scalawiki.dto.cmd.query.{Generator, Query}
import org.scalawiki.dto.cmd.query.prop._
import org.specs2.matcher.ThrownMessages
import org.specs2.mutable.Specification

class DslSpec extends Specification with ThrownMessages {

  "1" should {
    "23" in {
      val action = Action(
        Query(
          Prop(
            Info(InProp(SubjectId)),
            Revisions()
          )
        )
      )

      // action.flatten.map(_.name).toSet === Set("action", "prop", "inprop")
      action.query.toSeq.flatMap(_.props).map(_.name).toSet === Set(
        "info",
        "revisions"
      )
    }
  }

  "1" should {
    "23" in {
      val action = Action(
        Query(
          Prop(
            Info(InProp(SubjectId)),
            Revisions()
          )
        )
      )

      action.pairs.toMap === Map(
        "action" -> "query",
        "prop" -> "info|revisions",
        "inprop" -> "subjectid"
      )
    }
  }

  "2" should {
    "34" in {
      val action = Action(
        Query(
          Prop(
            Info(InProp(SubjectId)),
            Revisions()
          ),
          Generator(EmbeddedIn(EiTitle("Template:Name")))
        )
      )

      action.pairs.toMap === Map(
        "action" -> "query",
        "prop" -> "info|revisions",
        "inprop" -> "subjectid",
        "generator" -> "embeddedin",
        "geititle" -> "Template:Name"
      )
    }
  }

  "3" should {
    "34" in {
      val action = Action(
        Query(
          Prop(
            Info(InProp(SubjectId)),
            Revisions()
          ),
          Generator(CategoryMembers(CmTitle("Category:Name")))
        )
      )

      action.pairs.toMap === Map(
        "action" -> "query",
        "prop" -> "info|revisions",
        "inprop" -> "subjectid",
        "generator" -> "categorymembers",
        "gcmtitle" -> "Category:Name"
      )
    }
  }
}
