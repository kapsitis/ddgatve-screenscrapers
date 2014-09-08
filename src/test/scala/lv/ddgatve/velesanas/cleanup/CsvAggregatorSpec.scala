package lv.ddgatve.velesanas.cleanup

import org.specs2.mutable._
import lv.ddgatve.screenscrapers.csv.CSVWriter
import lv.ddgatve.screenscrapers.elections.SaeimaAggregateReader

class CsvAggregatorSpec extends Specification {
  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima-config.xml"

  "CsvAggregator" should {
    "Read Saeima11" in {
      val writer = new CsvAggregator(configFile, "saeima11")
      val aggregate = new SaeimaAggregateReader(11)
      val cReader = new ConfigurationReader(configFile, "saeima11")
      val urlPrefix = cReader.getUrlPrefix
      val urlAndProjections = aggregate.projectionsForAllParties(urlPrefix)
      //      val urlAndProjections = aggregate.projectionsForParty(1, urlPrefix).slice(0, 1)
      val stTables = writer.standardizeTable(urlAndProjections)
      CSVWriter.writeDatasets(11, stTables)
      0 mustEqual 0
    }

    "Read Saeima10" in {
      val writer = new CsvAggregator(configFile, "saeima10")
      val aggregate = new SaeimaAggregateReader(10)
      val cReader = new ConfigurationReader(configFile, "saeima10")
      val urlPrefix = cReader.getUrlPrefix
      val urlAndProjections = aggregate.projectionsForAllParties(urlPrefix)
      //      val urlAndProjections = aggregate.projectionsForParty(2, urlPrefix).slice(0, 1)
      val stTables = writer.standardizeTable(urlAndProjections)
      CSVWriter.writeDatasets(10, stTables)
      0 mustEqual 0
    }

    "Read Saeima09" in {
      val writer = new CsvAggregator(configFile, "saeima09")
      val aggregate = new SaeimaAggregateReader(9)
      val cReader = new ConfigurationReader(configFile, "saeima09")
      val urlPrefix = cReader.getUrlPrefix
      val urlAndProjections = aggregate.projectionsForAllParties(urlPrefix)
      //      val urlAndProjections = aggregate.projectionsForParty(2, urlPrefix).slice(0, 1)
      val stTables = writer.standardizeTable(urlAndProjections)
      CSVWriter.writeDatasets(9, stTables)
      0 mustEqual 0
    }

    "Read Saeima08" in {
      val writer = new CsvAggregator(configFile, "saeima08")
      val aggregate = new SaeimaAggregateReader(8)
      val cReader = new ConfigurationReader(configFile, "saeima08")
      val urlPrefix = cReader.getUrlPrefix
      val urlAndProjections = aggregate.projectionsForAllParties(urlPrefix)
      //      val urlAndProjections = aggregate.projectionsForParty(2, urlPrefix).slice(0, 1)
      val stTables = writer.standardizeTable(urlAndProjections)
      CSVWriter.writeDatasets(8, stTables)
      0 mustEqual 0
    }

    "Read Saeima07" in {
      val writer = new CsvAggregator(configFile, "saeima07")
      val aggregate = new SaeimaAggregateReader(7)
      val cReader = new ConfigurationReader(configFile, "saeima07")
      val urlPrefix = cReader.getUrlPrefix
      val urlAndProjections = aggregate.projectionsForAllParties(urlPrefix)
      //      val urlAndProjections = aggregate.projectionsForParty(2, urlPrefix).slice(0, 1)
      val stTables = writer.standardizeTable(urlAndProjections)
      CSVWriter.writeDatasets(7, stTables)
      0 mustEqual 0
    }

  }

}