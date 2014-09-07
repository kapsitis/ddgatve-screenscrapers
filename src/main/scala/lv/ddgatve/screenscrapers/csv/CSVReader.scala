package lv.ddgatve.screenscrapers.csv

import java.io.StringReader
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVParser
import org.apache.commons.csv.CSVRecord
import scala.collection.JavaConverters._
import java.io.FileReader

object CSVReader {
  def makeLine(arg: CSVRecord): List[String] = {
    val list = for (i <- 0 until arg.size()) yield {
      arg.get(i)
    }
    list.toList
  }

  def read(path: String): List[List[String]] = {
    val records = new CSVParser(
      new FileReader(path),
      CSVFormat.DEFAULT.withHeader()).asScala
    records.toList map makeLine
  }

  def readRecords(path: String): List[CSVRecord] = {
    val records = new CSVParser(
      new FileReader(path),
      CSVFormat.DEFAULT.withHeader()).asScala
    records.toList
  }
}