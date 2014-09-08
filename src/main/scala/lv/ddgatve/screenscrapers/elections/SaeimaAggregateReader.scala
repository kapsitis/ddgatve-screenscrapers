package lv.ddgatve.screenscrapers.elections

import lv.ddgatve.screenscrapers.csv.CSVReader

/**
 * saeima - Saeima order number (currently: 6,7,8,9,10 or 11)
 */
case class SaeimaAggregateReader(saeima: Int) {

  //  val saeimaCsvFiles = Map(6 -> "src/main/resources/saeima06.csv",
  //    7 -> "src/main/resources/saeima07.csv",
  //    8 -> "src/main/resources/saeima08.csv",
  //    9 -> "src/main/resources/saeima09.csv",
  //    10 -> "src/main/resources/saeima10.csv",
  //    11 -> "src/main/resources/saeima11.csv")

  //  val saeimaFields = Map("Saraksts" -> 1, "Rīga" -> 2, "Vidzeme" -> 3,
  //    "Latgale" -> 4, "Kurzeme" -> 5, "Zemgale" -> 6,
  //    "Kopā" -> 7, "Procenti" -> 8, "Vietas" -> 9, "Aploksnes" -> 10)

  val customUrlSuffixForSaeima07 = Map(
    "1" -> "14", "2" -> "15", "3" -> "20", "4" -> "5", "5" -> "19",
    "6" -> "12", "7" -> "6", "8" -> "13", "9" -> "2", "10" -> "9",
    "11" -> "8", "12" -> "17", "13" -> "18", "14" -> "10", "15" -> "3",
    "16" -> "16", "17" -> "1", "18" -> "7", "19" -> "21", "20" -> "4", "21" -> "11")

  val fname = f"src/main/resources/data-parties/saeima$saeima%02d.csv"

  val csvRecords = CSVReader.readRecords(fname)

  def getCsvRecords() = csvRecords
  def getLine(party: Int) = {
    if (party > 0) csvRecords(party - 1) else csvRecords(csvRecords.size - 2)
  }

  def getPartyField(party: Int, field: String): String = {
    val csvRecord = getLine(party)
    csvRecord.get(field)
  }

  val districts = Map(1 -> "Rīga", 2 -> "Vidzeme", 3 -> "Latgale", 4 -> "Kurzeme", 5 -> "Zemgale")

  /**
   * Find all the URLs that contain candidate data. Each URL is paired with
   * a map with some values that are constant within that URL.
   */
  def projectionsForParty(party: Int, urlPrefix: String): List[(String, Map[String, String])] = {
    val csvReader = SaeimaAggregateReader(saeima)
    val districtSuffixes = List(1, 2, 3, 4, 5)
    val allUrls = if (saeima == 7) {
      districtSuffixes map (urlPrefix +
        customUrlSuffixForSaeima07.get(party.toString).get + _.toString)
    } else {
      districtSuffixes map (urlPrefix + party.toString + _.toString)
    }
    // val allUrls = districtSuffixes map (urlPrefix + party.toString + _.toString)
    val allProjections = districtSuffixes map (distrNum => Map(
      "Saeima" -> saeima.toString,
      "Party" -> party.toString,
      "District" -> distrNum.toString,
      "BallotsForParty" -> csvReader.getPartyField(party.toInt, districts.get(distrNum).get)))
    allUrls zip allProjections
  }

  def findLargeParties(): List[Int] = {
    val aggregate = SaeimaAggregateReader(saeima)
    val records = aggregate.getCsvRecords.slice(0, aggregate.getCsvRecords.size - 2)
    val goodRecords = records filter (_.get("Procenti").toFloat >= 2.0F)
    goodRecords map (_.get("Nr").toInt)
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