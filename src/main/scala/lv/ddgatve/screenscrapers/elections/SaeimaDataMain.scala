package lv.ddgatve.screenscrapers.elections

import lv.ddgatve.velesanas.cleanup.ConfigurationReader

object SaeimaDataMain {

  val isTesting = true

  //  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima-config.xml"
  //  val workingDir = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/data/"

  //val saeimas = List(11, 10, 9, 8, 7, 6)
  val saeimas = List(11, 10, 9)
  val saeimaProfiles = Map(11 -> "saeima11", 10 -> "saeima10", 9 -> "saeima09", 8 -> "saeima08", 7 -> "saeima07")

  // test in Riga
  val testDistrict = 1
  // 11 - (1)Vienotība, 10 - (2)Vienotība, 9 - (2)SC, 
  // 8 - (1)TB/LNNK, 7 - (4)LC, 6 - (2)LZS/LKDS/LtgDemP
  val testParties = Map(11 -> 1, 10 -> 2, 9 -> 2, 8 -> 1, 7 -> 4, 6 -> 2)
  val individualsNames = List("Valid", "Modified", "Unmodified", "District", "Party")
  val outTableColumns = List("PostNum", "Candidate",
    "Sex", "NameOrigin", "PreNum",
    "Pluses", "Minuses", "NoMarks", "Points",
    "Result", "Saeima", "WhereCompeted",
    "Party", "District",
    "BallotsForParty")

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: lv.ddgatve.velesanas.cleanup.SaeimaDataMain <conf> <outDir>")
      System.exit(0)
    }
    val configFile = args(0)
    val workingDir = args(1)
    for (saeima <- saeimas) {
      // (1) consult aggregate data; look up download urls - assign to a list
      // (2) inject a dependency to ConfigurationReader(profile) and run downloader
      // (3) receive a raw+clean table(s) from the reader
      // (4) call a projection class to commpute projection of the current List[Map[String]]
      // (5) call a utility to record the result to a CSV
      val aggregate = new SaeimaAggregateReader(saeima)
      val cReader = new ConfigurationReader(configFile, saeimaProfiles.get(saeima).get)
      val urlPrefix = cReader.getUrlPrefix

      val urlAndProjections = if (isTesting) {
        aggregate.projectionsForParty(testParties.get(saeima).get, urlPrefix).slice(0, 1)
      } else {
        aggregate.projectionsForLargeParties(urlPrefix)
      }

      val f1 = f"singleCols$saeima%02d.csv" //"singleCols11.csv"
      val f2 = f"candidates$saeima%02d.csv"
      println(f"f1 = $f1%s, f2 = $f2%s")
      //      val writer = new CsvAggregator(configFile, "saeima11")
      //      writer.makeCSV(workingDir, f1, individualsNames, f2, outTableColumns, urlAndProjections)
    }
  }
}