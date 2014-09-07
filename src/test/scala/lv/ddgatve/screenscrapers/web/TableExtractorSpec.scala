package lv.ddgatve.screenscrapers.web

import org.specs2.mutable._

class TableExtractorSpec extends Specification {

  val url10A = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/komisijas2010.GalRez10.kandid?NR1=21"""
  val url09A = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/saeima9.GalRez9.kandid?NR1=195"""

  val url10B = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/komisijas2010.GalRez10.kandid?NR1=22"""

  val url08A = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/sae8dev.Vel8rmeg.kandid?NR1=11"""

  val url07A = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/base.vel7r.kandid?NR1=51"""
  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima-config.xml"

  "TableExtractor.extract for saeima10" should {
    val extractor = new TableExtractor(configFile, "saeima10")
    //    "Find 'Kristovskis'" in {
    //      val result = extractor.extract(url10A)
    //      val candidateTable = result.get("candidates").get
    //      candidateTable(0).get("Candidate").get mustEqual "Ģirts Valdis Kristovskis"
    //    }

    //    "Find 'Dombrovskis'" in {
    //      val result = extractor.extract(url10B)
    //      val candidateTable = result.get("candidates").get
    //      candidateTable(0).get("Candidate").get mustEqual "Valdis Dombrovskis"
    //    }
  }

  //  "TableExtractor.extract for saeima09" should {
  //    val extractor = new TableExtractor(configFile, "saeima09")
  //    "Find 'Dobelis'" in {
  //      val result = extractor.extract(url09A)
  //      val candidateTable = result.get("candidates").get
  //      candidateTable(0).get("Candidate").get mustEqual "Juris Dobelis"
  //    }
  //  }

  //  "TableExtractor.extract for saeima08" should {
  //    val extractor = new TableExtractor(configFile, "saeima08")
  //    "Find 'Krasts'" in {
  //      val result = extractor.extract(url08A)
  //      val candidateTable = result.get("candidates").get
  //      candidateTable(0).get("Candidate").get mustEqual "Guntars Krasts"
  //    }
  //  }

  "TableExtractor.extract for saeima07" should {
    val extractor = new TableExtractor(configFile, "saeima07")
    "Find 'Gorbunovs'" in {
      val result = extractor.extract(url07A)
      val candidateTable = result.get("candidates").get
      candidateTable(0).get("Candidate").get mustEqual "Anatolijs Gorbunovs"
    }
  }

  val extractor = new TableExtractor(configFile, "saeima11")

  "TableExtractor.extract" should {
    val Saeima11VienotibaRigaUrl = """http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.GalRez11.kandid?NR1=11&sec=2"""
    "Find 494779 for Saeima11.Vienotiba.Riga" in {
      val result = extractor.extract(Saeima11VienotibaRigaUrl)
      val singleColsTable = result.get("singleCols").get

      singleColsTable(0).get("Valid").get mustEqual "49479"
      singleColsTable(0).get("Modified").get mustEqual "32295"
      singleColsTable(0).get("Unmodified").get mustEqual "17184"
      singleColsTable(0).get("District").get mustEqual "Rīga"
      0 mustEqual 0
    }

    "Find votes for Kristovskis" in {
      val result = extractor.extract(Saeima11VienotibaRigaUrl)
      val candidatesTable = result.get("candidates").get
      candidatesTable(0).get("Candidate").get mustEqual "Andris Vilks"
      candidatesTable.size mustEqual 33
      candidatesTable(32).get("Candidate").get mustEqual "Ģirts Valdis Kristovskis"
    }
  }

}