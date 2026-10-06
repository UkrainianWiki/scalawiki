import sbt._

object Dependencies {

  val AaltoXmlV = "1.3.3"
  val BetterFilesV = "3.9.2"
  val BlameApiV = "15.10.15"
  val ChronicleMapV = "3.22.9"
  val ChronoScalaV = "1.0.0"
  val FicusV = "1.5.2"
  val GuavaV = "33.4.0-jre"
  val H2V = "1.4.200"
  val JimFsV = "1.3.0"
  val JodaTimeV = "2.13.0"
  val JSoupV = "1.18.3"
  val LogbackClassicV = "1.5.15"
  val MockServerV = "5.15.0"
  val ReactiveStreamsV = "1.0.4"
  val RetryV = "0.3.6"
  val Scala213V = "2.13.15"
  val ScalaChartV = "0.8.0"
  val ScalaCheckV = "1.18.1"
  val ScalaCsvV = "2.0.0"
  val ScalaParserCombinatorsV = "2.4.0"
  val ScalaXmlV = "2.3.0"
  val ScallopV = "5.2.0"
  val Slf4jV = "2.0.16"
  val SwcEngineV = "3.1.9"
  val TypesafeConfigV = "1.4.3"

  val resolvers = Seq(
    "Typesafe Repo" at "https://repo.typesafe.com/typesafe/releases/",
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases",
    Resolver.jcenterRepo,
    Resolver.bintrayRepo("rick-beton", "maven"),
    Resolver.bintrayRepo("softprops", "maven")
  )

  val overrides = Seq(
    Library.Pekko.actor,
    "org.reactivestreams" % "reactive-streams" % ReactiveStreamsV,
    "org.scala-lang.modules" %% "scala-xml" % ScalaXmlV,
    "org.scala-lang.modules" %% "scala-parser-combinators" % ScalaParserCombinatorsV,
    "com.google.guava" % "guava" % GuavaV,
    Library.Commons.codec,
    Library.Commons.compress,
    Library.Commons.io,
    Library.Commons.lang,
    "com.typesafe" % "config" % TypesafeConfigV,
    Library.Poi.ooxml,
    Library.Poi.poi,
    Library.Poi.ooxmlFull,
    Library.Jackson.core,
    Library.Jackson.annotations,
    Library.Jackson.databind,
    "joda-time" % "joda-time" % JodaTimeV,
    "org.slf4j" % "slf4j-api" % Slf4jV,
    "ch.qos.logback" % "logback-classic" % LogbackClassicV,
    "javax.xml.bind" % "jaxb-api" % "2.3.1",
    "org.apache.logging" % "log4j:log4j-core" % "2.18.1",
    "org.apache.logging" % "log4j:log4j-api" % "2.18.1"
  )

  object Library {

    object Pekko {
      val PekkoV = "1.0.3"
      val PekkoHttpV = "1.4.1"

      val actor = "org.apache.pekko" %% "pekko-actor" % PekkoV
      val stream = "org.apache.pekko" %% "pekko-stream" % PekkoV
      val http = "org.apache.pekko" %% "pekko-http" % PekkoHttpV
      val httpCaching = "org.apache.pekko" %% "pekko-http-caching" % PekkoHttpV
    }

    object Play {
      def PlayJsonV(isScala213: Boolean) = {
        if (isScala213) {
          "2.9.4"
        } else {
          "2.7.4" // scala-steward:off
        }
      }

      def TwirlV(isScala213: Boolean) = "1.5.2"

      def json(isScala213: Boolean) =
        "com.typesafe.play" %% "play-json" % PlayJsonV(isScala213)

      def twirlApi(isScala213: Boolean) =
        "com.typesafe.play" %% "twirl-api" % TwirlV(isScala213)
    }

    object Poi {
      val PoiV = "5.3.0"
      val PoiXwpfV = "2.1.0"

      val scratchpad = "org.apache.poi" % "poi-scratchpad" % PoiV
      val poi = "org.apache.poi" % "poi" % PoiV
      val ooxml = "org.apache.poi" % "poi-ooxml" % PoiV
      val ooxmlFull = "org.apache.poi" % "poi-ooxml-full" % PoiV

      val converter =
        "fr.opensagres.xdocreport" % "fr.opensagres.xdocreport.converter.docx.xwpf" % PoiXwpfV
    }

    object Commons {
      val CommonsCodecV = "1.17.1"
      val CommonsCompressV = "1.27.1"
      val CommonsLang3V = "3.17.0"
      val CommonsIoV = "2.18.0"

      val codec = "commons-codec" % "commons-codec" % CommonsCodecV
      val io = "commons-io" % "commons-io" % CommonsIoV
      val lang = "org.apache.commons" % "commons-lang3" % CommonsLang3V
      val compress =
        "org.apache.commons" % "commons-compress" % CommonsCompressV
    }

    object Jackson {
      val JacksonV = "2.18.2"

      val core = "com.fasterxml.jackson.core" % "jackson-core" % JacksonV
      val annotations =
        "com.fasterxml.jackson.core" % "jackson-annotations" % JacksonV
      val databind =
        "com.fasterxml.jackson.core" % "jackson-databind" % JacksonV
    }

    object Specs2 {
      val SpecsV = "4.20.9"

      val core = "org.specs2" %% "specs2-core" % SpecsV
      val matcherExtra = "org.specs2" %% "specs2-matcher-extra" % SpecsV
      val mock = "org.specs2" %% "specs2-mock" % SpecsV
    }

  }

}
