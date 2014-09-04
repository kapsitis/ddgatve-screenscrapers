package lv.ddgatve.velesanas.cleanup

import scala.io.Source
import lv.ddgatve.velesanas.csv.CSVReader

/**
 * saeima - Saeima order number
 * party - party number (or 0 - to get total results or something general)
 */
case class SaeimaAggregateReader(saeima: Int, party: Int) {

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

  //  val csvRecords = read(fname)
  val csvRecords = CSVReader.read(fname)

  val line = if (party > 0) csvRecords(party - 1) else csvRecords(csvRecords.size - 2)

  def getCsvRecords() = csvRecords
  def getLine() = line

  def getPartyField(field: String): String =
    line(saeimaFields.get(field).get)

  val districts = Map(1 -> "Rīga", 2 -> "Vidzeme", 3 -> "Latgale", 4 -> "Kurzeme", 5 -> "Zemgale")

  /**
   * Find all the URLs that contain candidate data. Each URL is paired with
   * a map with some values that are constant within that URL.
   */
  def projectionsForParty(urlPrefix: String): List[(String, Map[String, String])] = {
    val csvReader = SaeimaAggregateReader(saeima, party)
    val districtSuffixes = List(1, 2, 3, 4, 5)
    val allUrls = districtSuffixes map (urlPrefix + party.toString + _.toString)
    val allProjections = districtSuffixes map (distrNum => Map(
      "Saeima" -> saeima.toString,
      "Partija" -> party.toString,
      "Apgabals" -> distrNum.toString,
      "Balsis" -> csvReader.getPartyField(districts.get(distrNum).get)))
    allUrls zip allProjections
  }

  def findLargeParties(): List[Int] = {
    val aggregate = SaeimaAggregateReader(saeima, 0)
    val records = aggregate.getCsvRecords.slice(0, aggregate.getCsvRecords.size - 2)
    val goodRecords = records filter (_(8).toFloat >= 2.0F)
    goodRecords map (_(0).toInt)
  }

  def projectionsForLargeParties(urlPrefix: String): List[(String, Map[String, String])] = {
    val largeParties = findLargeParties()
    val pp = for (i <- largeParties) yield {
      projectionsForParty(urlPrefix)
    }
    pp.flatten
  }

  def projectionsForAllParties(urlPrefix: String): List[(String, Map[String, String])] = {
    val csvReader = SaeimaAggregateReader(saeima, 0)
    //val records = csvReader.getCsvRecords.slice(1, csvReader.getCsvRecords.size - 2)
    val pp = for (i <- 1 to csvReader.getCsvRecords.size - 3) yield {
      projectionsForParty(urlPrefix)
    }
    pp.toList.flatten
  }

}