package lv.ddgatve.velesanas.cleanup

import scala.io.Source
import lv.ddgatve.velesanas.csv.CSVReader

/**
 * saeima - Saeima order number (currently: 6,7,8,9,10 or 11)
 */
case class SaeimaAggregateReader(saeima: Int) {

  val saeimaCsvFiles = Map(6 -> "src/main/resources/saeima06.csv",
    7 -> "src/main/resources/saeima07.csv",
    8 -> "src/main/resources/saeima08.csv",
    9 -> "src/main/resources/saeima09.csv",
    10 -> "src/main/resources/saeima10.csv",
    11 -> "src/main/resources/saeima11.csv")

  val saeimaFields = Map("Saraksts" -> 1, "Rīga" -> 2, "Vidzeme" -> 3,
    "Latgale" -> 4, "Kurzeme" -> 5, "Zemgale" -> 6,
    "Kopā" -> 7, "Procenti" -> 8, "Vietas" -> 9, "Aploksnes" -> 10)

  val fname = saeimaCsvFiles.get(saeima).get

  val csvRecords = CSVReader.read(fname)

  def getCsvRecords() = csvRecords
  def getLine(party: Int) = {
    if (party > 0) csvRecords(party - 1) else csvRecords(csvRecords.size - 2)
  }

  def getPartyField(party: Int, field: String): String = {
    val line = getLine(party)
    line(saeimaFields.get(field).get)
  }

  val districts = Map(1 -> "Rīga", 2 -> "Vidzeme", 3 -> "Latgale", 4 -> "Kurzeme", 5 -> "Zemgale")

  /**
   * Find all the URLs that contain candidate data. Each URL is paired with
   * a map with some values that are constant within that URL.
   */
  def projectionsForParty(party: Int, urlPrefix: String): List[(String, Map[String, String])] = {
    val csvReader = SaeimaAggregateReader(saeima)
    val districtSuffixes = List(1, 2, 3, 4, 5)
    val allUrls = districtSuffixes map (urlPrefix + party.toString + _.toString)
    val allProjections = districtSuffixes map (distrNum => Map(
      "Saeima" -> saeima.toString,
      "Partija" -> party.toString,
      "Apgabals" -> distrNum.toString,
      "Balsis" -> csvReader.getPartyField(party.toInt, districts.get(distrNum).get)))
    allUrls zip allProjections
  }

  def findLargeParties(): List[Int] = {
    val aggregate = SaeimaAggregateReader(saeima)
    val records = aggregate.getCsvRecords.slice(0, aggregate.getCsvRecords.size - 2)
    val goodRecords = records filter (_(8).toFloat >= 2.0F)
    goodRecords map (_(0).toInt)
  }

  def projectionsForLargeParties(urlPrefix: String): List[(String, Map[String, String])] = {
    val largeParties = findLargeParties()
    val pp = for (i <- largeParties) yield {
      projectionsForParty(i, urlPrefix)
    }
    pp.flatten
  }

  def projectionsForAllParties(urlPrefix: String): List[(String, Map[String, String])] = {
    val pp = for (i <- 1 to getCsvRecords.size - 3) yield {
      projectionsForParty(i, urlPrefix)
    }
    pp.toList.flatten
  }

}