package lv.ddgatve.velesanas.cleanup

import scala.xml.XML
import scala.util.matching.Regex
import lv.ddgatve.screenscrapers.web.Downloader
import lv.ddgatve.screenscrapers.web.TableExtractor
import lv.ddgatve.screenscrapers.csv.CSVWriter

/**
 * This class is responsible for creating large CSV files from the files it visits.
 * It first creates "projections" - sets of URLs with additional fields specified in a map.
 * After that it aggregates everything that is in the projection and outputs to a CSV file.
 *
 * This class returns a data table similar to the one produced by an SQL or SPARQL query.
 * This class should NOT be specific to Saeima-related or any other data set.
 */
class CsvAggregator(configurationFile: String, profile: String) {
  val individualsNames = CSVWriter.individualsNames
  val tableColumns = CSVWriter.tableColumns

  private def safeGet(x: Map[String, String], y: String): String = {
    if (x.keySet contains y) {
      x.get(y).get
    } else {
      println("WARNING! Could not get key '" + y + "' from map '" + x)
      ""
    }
  }

  /**
   * Visit various urls with their respective preset values (projections).
   * Write the result into two separate CSV files - the "individuals" and the "table".
   */
  def standardizeTable(urlAndProjections: List[(String, Map[String, String])]): Map[String, List[List[String]]] = {
    val extractor = new TableExtractor(configurationFile, profile)
    var singleCols: List[List[String]] = List()
    var candidates: List[List[String]] = List()
    for (urlAndProjection <- urlAndProjections) {
      val downloadResult = extractor.extract(urlAndProjection._1)
      val projection = new Projection(List("singleCols", "candidates"), Map(
        "singleCols" -> individualsNames,
        "candidates" -> tableColumns))

      val theParty = urlAndProjection._2.get("Party").get
      val theSaeima = urlAndProjection._2.get("Saeima").get
      val temp = projection.standardize(downloadResult, Map(
        "singleCols" -> Map("Party" -> theParty, "Saeima" -> theSaeima),
        "candidates" -> urlAndProjection._2))
      val aa = temp.get("singleCols").get map (x => {
        individualsNames map (safeGet(x, _))
      })
      val bb = temp.get("candidates").get map (x => {
        tableColumns map (safeGet(x, _))
      })
      singleCols = singleCols ++: aa
      candidates = candidates ++: bb
    }
    Map("singleCols" -> singleCols, "candidates" -> candidates)
  }
}