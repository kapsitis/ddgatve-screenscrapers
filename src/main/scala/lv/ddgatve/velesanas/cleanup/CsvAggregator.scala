package lv.ddgatve.velesanas.cleanup

import scala.xml.XML
import scala.util.matching.Regex

/**
 * This class is responsible for creating large CSV files - candidate lists with points, etc.
 * It first creates "projections" - sets of URLs with additional fields specified in a map.
 * After that it aggregates everything that is in the projection and outputs to a CSV file.
 */
class CsvAggregator(configurationFile: String, profile: String) {

  val cReader = new ConfigurationReader(configurationFile, profile)
  val getPatternMap = cReader.getPatternMap
  val individualExtractors = cReader.getIndividualExtractors
  val tableExtractor = cReader.getTableExtractor
  val tableColumns = cReader.getTableColumns
  val tidyPatterns = cReader.getTidyPatterns
  val urlPrefix = cReader.getUrlPrefix

  /**
   * Visit various urls with their respective preset values (projections).
   * Write the result into two separate CSV files - the "individuals" and the "table".
   */
  def makeCSV(workingDirectory: String,
    individualsFileName: String,
    individualsNames: List[String],
    tableFileName: String,
    tableColumns: List[String],
    urlAndProjections: List[(String, Map[String, String])]): Unit = {
    val downloader = new lv.ddgatve.velesanas.cleanup.Downloader(
      individualExtractors.toList,
      tableExtractor,
      tableColumns.toList,
      tidyPatterns.toList)
    var individualLines: List[List[String]] = List()
    var tableLines: List[List[String]] = List()
    for (projection <- urlAndProjections) {
      Thread.sleep(1000)
      println("Downloading URL " + projection._1)
      val downloadResult = downloader.extract(projection._1)
      val aa = individualsNames map { (downloadResult._1).get(_).get }
      individualLines = individualLines :+ aa
      val bb = downloadResult._2 map { mm =>
        tableColumns map {
          colName =>
            {
              if (projection._2.keySet contains (colName)) {
                println("colName(projection) is " + colName)
                projection._2.get(colName).get
              } else {
                println("colName(table) is " + colName)
                mm.get(colName).get
              }
            }
        }
      }
      tableLines = tableLines ++: bb
    }

    CsvWriter.write(workingDirectory + individualsFileName, individualsNames, individualLines)
    CsvWriter.write(workingDirectory + tableFileName, tableColumns, tableLines)

  }
}