package lv.ddgatve.screenscrapers.elections

import org.specs2.mutable.Specification

class SaeimaAggregateReaderSpec extends Specification {
  val urlPrefix = "http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1="

  val saraksti = Map(6 -> 19, 7 -> 21, 8 -> 20, 9 -> 19, 10 -> 13, 11 -> 13)

  "SaeimaAggregateReader filesizes" should {
    "process saeima06" in {
      SaeimaAggregateReader(6).csvRecords.size mustEqual saraksti.get(6).get + 2
    }
    "process saeima07" in {
      SaeimaAggregateReader(7).csvRecords.size mustEqual saraksti.get(7).get + 2
    }
    "process saeima08" in {
      SaeimaAggregateReader(8).csvRecords.size mustEqual saraksti.get(8).get + 2
    }
    "process saeima09" in {
      SaeimaAggregateReader(9).csvRecords.size mustEqual saraksti.get(9).get + 2
    }
    "process saeima10" in {
      SaeimaAggregateReader(10).csvRecords.size mustEqual saraksti.get(10).get + 2
    }
    "process saeima11" in {
      SaeimaAggregateReader(11).csvRecords.size mustEqual saraksti.get(11).get + 2
    }
  }

  "SaeimaAggregateReader for Saeima10.Vienotiba" should {
    val csvReader = SaeimaAggregateReader(10)
    val line = csvReader.getLine(2)
    "Find saeima10.vienotiba.vidzeme in a line" in {
      line.get("Vidzeme") mustEqual 110194.toString
    }
    "Find saeima10.vienotiba.vidzeme from a field" in {
      csvReader.getPartyField(2, "Vidzeme") mustEqual 110194.toString
    }
    "Find saeima10.vienotiba.procenti" in {
      csvReader.getPartyField(2, "Procenti") mustEqual "31.219"
    }
  }

  "SaeimaAggregateReader for Saeima10.Total" should {
    val csvReader = SaeimaAggregateReader(10)
    val line = csvReader.getLine(0)
    "Find saeima10.aploksnes" in {
      csvReader.getPartyField(0, "Aploksnes") mustEqual 965538.toString
    }
    "Find saeima10.deputati.riga" in {
      csvReader.getCsvRecords.last.get("Rīga") mustEqual 29.toString
    }
  }

  "SaeimaAggregateReader for saeima08.tblnnk" should {
    val aggregate = SaeimaAggregateReader(8)
    "Find procenti > 5%" in {
      aggregate.getPartyField(1, "Procenti") mustEqual "5.366"
    }
  }

  "SaeimaAggregateReader.projectionsForParty" should {
    val aggregate = SaeimaAggregateReader(10)
    val result = aggregate.projectionsForParty(2,
      """http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=""")
    "find saeima10.vienotiba.Latgale URL" in {
      result(2)._1 mustEqual """http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=23"""
    }
    "find saeima10.vienotiba.Latgale Projection" in {
      result(2)._2 mustEqual Map("Saeima" -> "10", "Party" -> "2",
        "BallotsForParty" -> "19231", "District" -> "3")
    }

  }

  "SaeimaAggregateReader.findLargeParties" should {
    "find large in saeima06" in {
      val aggregate = SaeimaAggregateReader(6)
      aggregate.findLargeParties() mustEqual List(2, 4, 5, 6, 9, 10, 13, 14, 17, 18)
    }
    "find large in saeima07" in {
      val aggregate = SaeimaAggregateReader(7)
      aggregate.findLargeParties() mustEqual List(4, 6, 7, 8, 9, 14, 15, 17)
    }
    "find large in saeima08" in {
      val aggregate = SaeimaAggregateReader(8)
      aggregate.findLargeParties() mustEqual List(1, 2, 5, 7, 8, 9, 15, 20)
    }
    "find large in saeima09" in {
      val aggregate = SaeimaAggregateReader(9)
      aggregate.findLargeParties() mustEqual List(2, 3, 5, 7, 9, 10, 14, 15, 19)
    }
    "find large in saeima10" in {
      val aggregate = SaeimaAggregateReader(10)
      aggregate.findLargeParties() mustEqual List(2, 4, 6, 8, 12)
    }
    "find large in saeima11" in {
      val aggregate = SaeimaAggregateReader(11)
      aggregate.findLargeParties() mustEqual List(1, 3, 5, 6, 8, 11)
    }
  }

  "SaeimaAggregateReader.projectionsForLargeParties" should {
    "return list with 5 elements" in {
      val aggregate = SaeimaAggregateReader(11)
      val result = aggregate.projectionsForLargeParties(urlPrefix)
      result.size mustEqual 30
      result(0)._1 must endWith("11")
      result(1)._1 must endWith("12")
      result(2)._1 must endWith("13")
      result(3)._1 must endWith("14")
      result(4)._1 must endWith("15")
    }
  }

  "SaeimaAggregateReader.getCsvRecords" should {
    "find 20 parties in Saeima8" in {
      val aggregate = SaeimaAggregateReader(8)
      aggregate.getCsvRecords.size mustEqual 22
    }
  }

  "SaeimaAggregateReader.projectionsForAllParties" should {
    "find 19 parties in Saeima6" in {
      val aggregate = SaeimaAggregateReader(6)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 19
    }
    "find 21 parties in Saeima7" in {
      val aggregate = SaeimaAggregateReader(7)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 21
    }

    "find 20 parties in Saeima8" in {
      val aggregate = SaeimaAggregateReader(8)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 20
    }
    "find 19 parties in Saeima9" in {
      val aggregate = SaeimaAggregateReader(9)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 19
    }
    "find 13 parties in Saeima10" in {
      val aggregate = SaeimaAggregateReader(10)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 13
    }
    "find 13 parties in Saeima11" in {
      val aggregate = SaeimaAggregateReader(11)
      val result = aggregate.projectionsForAllParties(urlPrefix)
      result.size mustEqual 5 * 13
    }

  }

}