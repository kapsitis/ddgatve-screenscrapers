package lv.ddgatve.velesanas.cleanup

import org.specs2.mutable._

class SaeimaAggregateReaderSpec extends Specification {
  val urlPrefix = "http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1="

  val saraksti = Map(6 -> 19, 7 -> 21, 8 -> 20, 9 -> 19, 10 -> 13, 11 -> 13)

  "SaeimaAggregateReader filesizes" should {
    "process saeima06" in {
      SaeimaAggregateReader(6, 0).csvRecords.size mustEqual saraksti.get(6).get + 2
    }
    "process saeima07" in {
      SaeimaAggregateReader(7, 0).csvRecords.size mustEqual saraksti.get(7).get + 2
    }
    "process saeima08" in {
      SaeimaAggregateReader(8, 0).csvRecords.size mustEqual saraksti.get(8).get + 2
    }
    "process saeima09" in {
      SaeimaAggregateReader(9, 0).csvRecords.size mustEqual saraksti.get(9).get + 2
    }
    "process saeima10" in {
      SaeimaAggregateReader(10, 0).csvRecords.size mustEqual saraksti.get(10).get + 2
    }
    "process saeima11" in {
      SaeimaAggregateReader(11, 0).csvRecords.size mustEqual saraksti.get(11).get + 2
    }
  }

  "SaeimaAggregateReader for Saeima10.Vienotiba" should {
    val csvReader = SaeimaAggregateReader(10, 2)
    val line = csvReader.getLine
    "Find saeima10.vienotiba.vidzeme in a line" in {
      line(3) mustEqual 110194.toString
    }
    "Find saeima10.vienotiba.vidzeme from a field" in {
      csvReader.getPartyField("Vidzeme") mustEqual 110194.toString
    }
    "Find saeima10.vienotiba.procenti" in {
      csvReader.getPartyField("Procenti") mustEqual "31.219"
    }
  }

  "SaeimaAggregateReader for Saeima10.Total" should {
    val csvReader = SaeimaAggregateReader(10, 0)
    val line = csvReader.getLine
    "Find saeima10.aploksnes" in {
      csvReader.getPartyField("Aploksnes") mustEqual 965538.toString
    }
    "Find saeima10.deputati.riga" in {
      csvReader.getCsvRecords.last(2) mustEqual 29.toString
    }
  }

  "SaeimaAggregateReader for saeima08.tblnnk" should {
    val aggregate = SaeimaAggregateReader(8, 1)
    "Find procenti > 5%" in {
      aggregate.getPartyField("Procenti") mustEqual "5.366"
    }
  }

  "SaeimaAggregateReader.projectionsForParty" should {
    val csvReader = SaeimaAggregateReader(11, 0)
    //    val aggregator = new CsvAggregator(configFile, "saeima11")
    val result = csvReader.projectionsForParty(
      """http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=""")
    "find saeima10.vienotiba.Latgale URL" in {
      result(2)._1 mustEqual """http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=23"""
    }
    "find saeima10.vienotiba.Latgale Projection" in {
      result(2)._2 mustEqual Map("Saeima" -> "10", "Partija" -> "2",
        "Balsis" -> "19231", "Apgabals" -> "3")
    }

  }

  "SaeimaAggregateReader.findLargeParties" should {
    //    val aggregator = new CsvAggregator(configFile, "saeima06")
    "find large in saeima06" in {
      val aggregate = SaeimaAggregateReader(6, 0)
      aggregate.findLargeParties() mustEqual List(2, 4, 5, 6, 9, 10, 13, 14, 17, 18)
    }
    "find large in saeima07" in {
      val aggregate = SaeimaAggregateReader(7, 0)
      aggregate.findLargeParties() mustEqual List(4, 6, 7, 8, 9, 14, 15, 17)
    }
    "find large in saeima08" in {
      val aggregate = SaeimaAggregateReader(8, 0)
      aggregate.findLargeParties() mustEqual List(1, 2, 5, 7, 8, 9, 15, 20)
    }
    "find large in saeima09" in {
      val aggregate = SaeimaAggregateReader(9, 0)
      aggregate.findLargeParties() mustEqual List(2, 3, 5, 7, 9, 10, 14, 15, 19)
    }
    "find large in saeima10" in {
      val aggregate = SaeimaAggregateReader(10, 0)
      aggregate.findLargeParties() mustEqual List(2, 4, 6, 8, 12)
    }
    "find large in saeima11" in {
      val aggregate = SaeimaAggregateReader(11, 0)
      aggregate.findLargeParties() mustEqual List(1, 3, 5, 6, 8, 11)
    }
  }

  "SaeimaAggregateReader.projectionsForLargeParties" should {
    "return list with 5 elements" in {
      val aggregate = SaeimaAggregateReader(11, 0)
      val result = aggregate.projectionsForLargeParties(urlPrefix)
      result.size mustEqual 30
      result(0)._1 must endWith("11")
      result(1)._1 must endWith("12")
      result(2)._1 must endWith("13")
      result(3)._1 must endWith("14")
      result(4)._1 must endWith("15")
    }
  }

}