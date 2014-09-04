package lv.ddgatve.velesanas.cleanup

import org.specs2.mutable._

class CsvAggregatorSpec extends Specification {
  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/test/resources/saeima-config.xml"
  val urlPrefix = "http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1="
  val parties = List((1, "Vienotiba"))

  "CsvAggregator" should {
    "Read Saeima11-syntax data" in {
      val writer = new CsvAggregator(configFile, "saeima11")
      val workingDir = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/"
      val f1 = "individuals.csv"
      val f2 = "table.csv"
      val individualsNames = List("Derigas", "Grozitas", "Negrozitas", "Apgabals")
      val tableColumns = List("PostNum", "Kandidats",
        "PreNum", "Plusi",
        "Svitrojumi", "Punkti",
        "Rezultats", "Saeima",
        "Partija", "Apgabals",
        "Balsis")

      val aggregate = new SaeimaAggregateReader(11, 2)
      val urlAndProjections = aggregate.projectionsForParty(urlPrefix)
      writer.makeCSV(workingDir, f1, individualsNames, f2, tableColumns, urlAndProjections)

      0 mustEqual 0
    }
  }

}