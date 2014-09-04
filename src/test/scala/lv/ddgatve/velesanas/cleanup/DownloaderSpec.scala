package lv.ddgatve.velesanas.cleanup

import org.specs2.mutable._

class DownloaderSpec extends Specification {

  val Saeima11VienotibaRigaUrl = "http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=11"
  val regexTable = """(?s)(<TABLE BORDER=1>(.*)</TABLE>)""".r
  val fields = List("PreNum", "Kandidats", "Points", "Plusi", "Svitrojumi", "NavAtzimju", "Rezultats")

  val uniqueFieldRegex1 = """\(Derīgās zīmes - <b>([0-9]+)</b>, grozītas - ([0-9]+), negrozītas - ([0-9]+)\)""".r
  val uniqueFieldRegex2 = """>([^<>]+)s apgabals<""".r
  val uniqueFieldExtractors = List(("Derigas", uniqueFieldRegex1, 1),
    ("Grozitas", uniqueFieldRegex1, 2),
    ("Negrozitas", uniqueFieldRegex1, 3),
    ("Apgabals", uniqueFieldRegex2, 1))
  //  val uniqueFields = List("Derigas", "Grozitas", "Negrozitas", "Apgabals")

  val r1 = """( BORDER=1| align=(center|left)| bgcolor=("white"|"#f4f4e6")| color=("#003333"|"#003333")|&nbsp;|</a>|<CENTER>)""".r
  val r2 = """(?s)<Tr .*?</TR>""".r
  val r3 = """(<font >|</font>)""".r
  val r4 = """<tr>""".r
  val tidyTable = List((r1, -1, ""), (r2, 1, ""), (r3, -1, ""), (r4, -1, "<TR>"))

  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima-config.xml"
  val cr = new ConfigurationReader(configFile, "saeima11")

  "Saeima11.Vienotiba.Riga" should {
    "Find 494779 Total Valid Ballots" in {
      val downloader = new Downloader(cr.getIndividualExtractors().toList,
        cr.getTableExtractor,
        cr.getTableColumns.toList,
        cr.getTidyPatterns.toList)
      val result = downloader.extract(Saeima11VienotibaRigaUrl)
      result._1.get("Derigas").get mustEqual "49479"
      result._1.get("Grozitas").get mustEqual "32295"
      result._1.get("Negrozitas").get mustEqual "17184"
      result._1.get("Apgabals").get mustEqual "Rīga"
    }

    "Find votes for Kristovskis" in {
      val downloader = new Downloader(uniqueFieldExtractors,
        regexTable,
        fields,
        tidyTable)
      val result = downloader.extract(Saeima11VienotibaRigaUrl)
      result._2(0).get("Kandidats").get mustEqual "Ģirts Valdis Kristovskis"
      result._2.size mustEqual 33
      result._2(32).get("Kandidats").get mustEqual "Veiko Spolītis"
    }
  }
}
