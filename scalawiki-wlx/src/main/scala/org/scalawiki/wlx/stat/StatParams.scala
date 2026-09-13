package org.scalawiki.wlx.stat

import org.scalawiki.wlx.stat.rating.RateConfig

import java.time.ZonedDateTime

case class StatConfig(
    campaign: String,
    years: Seq[Int] = Nil,
    regions: Seq[String] = Nil,
    exceptRegions: Seq[String] = Nil,
    cities: Seq[String] = Nil,
    exceptCities: Seq[String] = Nil,
    rateConfig: RateConfig = RateConfig(),
    gallery: Boolean = false,
    fillLists: Boolean = false,
    fillListsRating: Boolean = false,
    wrongIds: Boolean = false,
    missingIds: Boolean = false,
    multipleIds: Boolean = false,
    lowRes: Boolean = false,
    specialNominations: Boolean = false,
    regionalStat: Boolean = false,
    regionalDetails: Boolean = false,
    authorsStat: Boolean = false,
    regionalGallery: Boolean = false,
    missingGallery: Boolean = false,
    placeDetection: Boolean = false,
    newMonuments: Boolean = false,
    rateInputDistribution: Boolean = false,
    mostPopularMonuments: Boolean = false,
    minMpx: Option[Float] = None,
    previousYearsGallery: Boolean = false,
    numberOfMonumentsByNumberOfPictures: Boolean = false,
    recentlyTakenFiles: Option[String] = None,
    exportCsv: Option[String] = None,
    exportImagesCsv: Option[String] = None,
    imagesFromCsv: Option[String] = None,
    csvCache: Boolean = true,
    csvCacheDir: String = "csv-cache",
    csvCacheRefresh: Boolean = false,
    csvCacheResync: Boolean = false,
    monumentCacheRefresh: Boolean = false,
    verbose: Boolean = false,
    progress: Boolean = true
) {

  /** Directory holding the automatic image CSV cache. An explicit
    * `--images-from-csv` dir wins (and keeps its strict "must exist" semantics);
    * otherwise the `csv-cache/` subdir of the working directory.
    */
  def effectiveCsvCacheDir: String = imagesFromCsv.getOrElse(csvCacheDir)
}

import org.rogach.scallop._

class StatParams(arguments: Seq[String]) extends ScallopConf(arguments) {
  val years = opt[List[Int]]("year", 'y', "contest year(s).")
  val startYear = opt[Int]("start-year", 's', "start contest year.")
  val campaign = opt[String](
    "campaign",
    'c',
    "upload campaign, like wlm-ua.",
    required = true
  )
  val regions = opt[List[String]]("region", 'r', "region code")
  val exceptRegions =
    opt[List[String]](name = "except-regions", descr = "except region codes")
  val cities = opt[List[String]](name = "cities", descr = "cities")
  val exceptCities =
    opt[List[String]](name = "except-cities", descr = "except cities")
  val newObjectRating =
    opt[Double](name = "new-object-rating", descr = "new object rating")
  val newAuthorObjectRating = opt[Double](
    name = "new-author-object-rating",
    descr = "new author object rating"
  )
  val numberOfAuthorsBonus = opt[Boolean](
    name = "number-of-authors-bonus",
    descr = "number of authors bonus"
  )
  val numberOfImagesBonus = opt[Boolean](
    name = "number-of-images-bonus",
    descr = "number of images bonus"
  )
  val baseRate = opt[Double](name = "base-rate", descr = "base rate")
  val gallery = opt[Boolean](name = "gallery", descr = "gallery")
  val fillLists = opt[Boolean](name = "fill-lists", descr = "fill lists")
  val fillListsRating = opt[Boolean](
    name = "fill-lists-rating",
    descr =
      "fill the rating (бали) field in monument lists with the points a new photo would score; run with a year range"
  )
  val wrongIds = opt[Boolean](name = "wrong-ids", descr = "report wrong ids")
  val missingIds =
    opt[Boolean](name = "missing-ids", descr = "report missing ids")
  val multipleIds =
    opt[Boolean](name = "multiple-ids", descr = "report multiple ids")
  val lowRes =
    opt[Boolean](name = "low-res", descr = "report low resolution photos")
  val specialNominations = opt[Boolean](
    name = "special-nominations",
    descr = "report special nominations"
  )
  val regionalStat =
    opt[Boolean](name = "regional-stat", descr = "report regional statistics")
  val regionalDetails = opt[Boolean](
    name = "regional-details",
    descr =
      "for each oblast, report a current-totals breakdown by today's raions and hromadas, nested in that oblast's detail page"
  )
  val authorsStat =
    opt[Boolean](name = "authors-stat", descr = "report authors statistics")
  val regionalGallery =
    opt[Boolean](name = "regional-gallery", descr = "report regional gallery")
  val missingGallery =
    opt[Boolean](name = "missing-gallery", descr = "report missing galleries")
  val placeDetection =
    opt[Boolean](name = "place-detection", descr = "report place detection")
  val newMonuments =
    opt[Boolean](name = "new-monuments", descr = "new monuments")
  val rateInputDistribution = opt[Boolean](
    name = "rate-input-distribution",
    descr = "rate input distribution"
  )
  val mostPopularMonuments = opt[Boolean](
    name = "most-popular-monuments",
    descr = "most popular monuments"
  )
  val minMpx = opt[Float](name = "min-mpx", descr = "minimum megapixels")
  val previousYearsGallery =
    opt[Boolean](name = "prev-years-gallery", descr = "previous years gallery")

  val recentlyTakenFiles =
    opt[String](name = "recently-taken-files", descr = "recently taken files")
  val exportCsv =
    opt[String](name = "export-csv", descr = "Export monuments to CSV. Optional filename; defaults to <campaign>-YYYY-MM-DD-HHmm.csv")
  val exportImagesCsv =
    opt[String](name = "export-images-csv", descr = "Export images to CSV files per year. Argument is output directory (default: current dir).")
  val imagesFromCsv =
    opt[String](name = "images-from-csv", descr = "Read past years' images from CSV files instead of querying/caching from the wiki. Argument is the directory containing <campaign>-<year>-images.csv / <campaign>-all-images.csv files produced by --export-images-csv.")
  val noCsvCache =
    opt[Boolean](name = "no-csv-cache", descr = "Disable the automatic image CSV cache (csv-cache/ dir); always fetch/parse from the wiki (the http-cache/ request cache still applies).")
  val csvCacheDir =
    opt[String](name = "csv-cache-dir", descr = "Directory for the automatic image CSV cache (default: csv-cache).")
  val csvCacheRefresh =
    opt[Boolean](name = "csv-cache-refresh", descr = "Ignore existing image CSV caches this run: refetch from the wiki and overwrite them.")
  val csvCacheResync =
    opt[Boolean](name = "csv-cache-resync", descr = "Re-check cached images (past years + all-images) against the wiki via a cheap id+revision sweep: refetch only rows whose file page changed since caching, drop deleted/de-categorised ones. The current year's cache always does this.")
  val monumentCacheRefresh =
    opt[Boolean](name = "monument-cache-refresh", descr = "Ignore the cached monument lists (csv-cache/<campaign>-monuments.csv) this run: refetch every list page from the wiki and overwrite the cache. Without this the cache is kept and only pages whose revision changed are refetched.")
  val verbose =
    opt[Boolean](name = "verbose", descr = "Verbose logging: echo per-request INFO detail to the console and record DEBUG detail in logs/scalawiki.log (default: console WARN+, file INFO).")
  val noProgress =
    opt[Boolean](name = "no-progress", descr = "Disable the live console progress display (progress is still written to logs/scalawiki.log).")
  verify()

}

object StatParams {

  def parse(args: Seq[String]): StatConfig = {

    val conf = new StatParams(args)

    val year = conf.years.getOrElse(List(ZonedDateTime.now.getYear)).sorted
    val startYear = conf.startYear.getOrElse(year.head)

    val years = (startYear to year.last).toList

    StatConfig(
      campaign = conf.campaign(),
      years = years,
      regions = conf.regions.getOrElse(Nil),
      exceptRegions = conf.exceptRegions.getOrElse(Nil),
      cities = conf.cities.getOrElse(Nil),
      exceptCities = conf.exceptCities.getOrElse(Nil),
      rateConfig = rating.RateConfig(conf),
      gallery = conf.gallery.getOrElse(false),
      fillLists = conf.fillLists.getOrElse(false),
      fillListsRating = conf.fillListsRating.getOrElse(false),
      wrongIds = conf.wrongIds.getOrElse(false),
      missingIds = conf.missingIds.getOrElse(false),
      multipleIds = conf.multipleIds.getOrElse(false),
      lowRes = conf.lowRes.getOrElse(false),
      specialNominations = conf.specialNominations.getOrElse(false),
      regionalStat = conf.regionalStat.getOrElse(false),
      regionalDetails = conf.regionalDetails.getOrElse(false),
      authorsStat = conf.authorsStat.getOrElse(false),
      regionalGallery = conf.regionalGallery.getOrElse(false),
      missingGallery = conf.missingGallery.getOrElse(false),
      placeDetection = conf.placeDetection.getOrElse(false),
      newMonuments = conf.newMonuments.getOrElse(false),
      rateInputDistribution = conf.rateInputDistribution.getOrElse(false),
      mostPopularMonuments = conf.mostPopularMonuments.getOrElse(false),
      minMpx = conf.minMpx.toOption,
      previousYearsGallery = conf.previousYearsGallery.getOrElse(false),
      recentlyTakenFiles = conf.recentlyTakenFiles.toOption,
      exportCsv = conf.exportCsv.toOption,
      exportImagesCsv = conf.exportImagesCsv.toOption,
      imagesFromCsv = conf.imagesFromCsv.toOption,
      csvCache = !conf.noCsvCache.getOrElse(false),
      csvCacheDir = conf.csvCacheDir.getOrElse("csv-cache"),
      csvCacheRefresh = conf.csvCacheRefresh.getOrElse(false),
      csvCacheResync = conf.csvCacheResync.getOrElse(false),
      monumentCacheRefresh = conf.monumentCacheRefresh.getOrElse(false),
      verbose = conf.verbose.getOrElse(false),
      progress = !conf.noProgress.getOrElse(false)
    )
  }
}
