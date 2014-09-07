package lv.ddgatve.screenscrapers.csv

import org.specs2.mutable.Specification

class CSVWriterSpec extends Specification {

  "CsvWriter" should {
    "Write a simple file ff.txt" in {
      CSVWriter.write("/home/kalvis/Desktop/elections/ff.txt",
        List("aaa", "bbb eee", "ddd"),
        List(List("1", "2", "3"), List("3", "4", "5")))
      0 mustEqual 0
    }
  }

}