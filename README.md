# scalawiki
<img src="https://github.com/UkrainianWiki/scalawiki/blob/master/resources/scalawiki.png?raw=true"  align="right" width="100" />
scalawiki is an experimental MediaWiki client in Scala on early stages of development.



[![CI](https://github.com/UkrainianWiki/scalawiki/actions/workflows/ci.yml/badge.svg)](https://github.com/UkrainianWiki/scalawiki/actions/workflows/ci.yml)
[![codecov.io](https://codecov.io/github/UkrainianWiki/scalawiki/coverage.svg?branch=master)](https://codecov.io/github/UkrainianWiki/scalawiki?branch=master)

**Requirements:** Java 17 or later, to build and to use the jars, which are compiled for Java 17. Scala 2.13, sbt. See [RUNNING.md](RUNNING.md) to run the WLM/WLE statistics.


Why [another client library for MediaWiki](https://www.mediawiki.org/wiki/API:Client_code)?

I didn't know any Java client that supported [generators](https://www.mediawiki.org/wiki/API:Query#Generators) (fetching properties from articles listed by list query in a single request). JWBF [only recently] (https://github.com/eldur/jwbf/issues/21) got the ability to query more than 1 page at a time. 

When Wikipedia sites are real Big Data it is just a show stopper. Fetching information about Wiki Loves Monuments uploads in such ineffective way will take almost a day even for one country, when could be done in several minutes otherwise in batches.

This library uses [Scala Futures](http://docs.scala-lang.org/overviews/core/futures.html) for easy job parallelization.

# Goals
  * Fully support [MediaWiki API](https://www.mediawiki.org/wiki/API:Main_page)
  * Support different backends - MediaWiki API, [xml dumps](https://meta.wikimedia.org/wiki/Data_dumps), [MediWiki database](https://www.mediawiki.org/wiki/Manual:Database_layout). Support copying data between backends (importing and exporting xml dumps to database, storing data retrived by MediaWiki API to xml dumps or database).
  * Good test coverage
