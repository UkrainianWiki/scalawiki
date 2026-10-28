package net.spraycookies.tldlist

import org.scalacheck.Prop._
import org.scalacheck.Properties

object EffectiveTldListSpecification extends Properties("EffectiveTldListSpecification") {

  property("contains") = forAll { (a: String, b: String) ⇒
    val tldList = new TrieTldList {
      val domainTrie = TldTrie(List(a).iterator)
    }

    tldList.contains(a) && (b == "" || !tldList.contains(b + a))
  }
}
