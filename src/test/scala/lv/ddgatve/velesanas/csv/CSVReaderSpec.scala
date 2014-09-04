package lv.ddgatve.velesanas.csv

import org.specs2.mutable._

class CSVReaderSpec extends Specification {

  val path = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima11.csv"
  val testFields = List("Nr", "Saraksts",
    "Rīga", "Vidzeme", "Latgale", "Kurzeme", "Zemgale",
    "Kopā", "Procenti", "Vietas", "Aploksnes")
  "CSVReader.read" should {
    "find 15 lines in saeima11.csv" in {
      val lines = CSVReader.read(path)
      lines.size mustEqual 15
    }
    "read saeima11.csv" in {
      val lines = CSVReader.read(path)
      lines(12)(1) mustEqual "Brīvība. Brīvs no bailēm, naida un dusmām"
    }
  }
}