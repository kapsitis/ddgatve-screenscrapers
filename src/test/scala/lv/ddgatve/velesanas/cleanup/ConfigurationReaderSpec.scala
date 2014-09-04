package lv.ddgatve.velesanas.cleanup

import org.specs2.mutable._

class ConfigurationReaderSpec extends Specification {
  val configFile = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/saeima-config.xml"

  val regexTable = """(?s)(<TABLE BORDER=1>(.*)</TABLE>)""".r
  val fields = List("PreNum", "Kandidats", "Points", "Plusi", "Svitrojumi", "NavAtzimju", "Rezultats")

  val uniqueFieldRegex1 = """\(Derīgās zīmes - <b>([0-9]+)</b>, grozītas - ([0-9]+), negrozītas - ([0-9]+)\)""".r
  val uniqueFieldRegex2 = """>([^<>]+)s apgabals<""".r
  val myIndividualExtractors = List(("Derigas", uniqueFieldRegex1, 1),
    ("Grozitas", uniqueFieldRegex1, 2),
    ("Negrozitas", uniqueFieldRegex1, 3),
    ("Apgabals", uniqueFieldRegex2, 1))
  val myIndividualFields = List("Derigas", "Grozitas", "Negrozitas", "Apgabals")

  val r1 = """( BORDER=1| align=(center|left)| bgcolor=("white"|"#f4f4e6")| color=("#003333"|"#003333")|&nbsp;|</a>|<CENTER>)""".r
  val r2 = """(?s)<Tr .*?</TR>""".r
  val r3 = """(<font >|</font>)""".r
  val r4 = """<tr>""".r
  val tidyTable = List((r1, -1, ""), (r2, 1, ""), (r3, -1, ""), (r4, -1, "<TR>"))

  "ConfigurationReader" should {
    "import patterns" in {
      val cr = new ConfigurationReader(configFile, "saeima11")
      cr.getPatternMap.size mustEqual 7
      cr.getPatternMap.get("uniqueFieldRegex1").get.toString mustEqual
        """\(Derīgās zīmes - <b>([0-9]+)</b>, grozītas - ([0-9]+), negrozītas - ([0-9]+)\)"""
      cr.getPatternMap.get("clean4").get.toString mustEqual "<tr>"

    }
    "find profile" in {
      val cr = new ConfigurationReader(configFile, "saeima11")
      val pNode = cr.getProfile
      pNode.toString must startWith("<profile")
    }
    "read individual fields" in {
      val cr = new ConfigurationReader(configFile, "saeima11")
      val fields = (cr.getIndividualExtractors map { _._1 }).toList
      fields mustEqual myIndividualFields
    }

    "read individual group numbers" in {
      val cr = new ConfigurationReader(configFile, "saeima11")
      val individualExtractors = (cr.getIndividualExtractors map { _._3 }).toList
      individualExtractors mustEqual List(1, 2, 3, 1)
    }

    "read individual regexes" in {
      val cr = new ConfigurationReader(configFile, "saeima11")
      val individualExtractors = (cr.getIndividualExtractors map { _._2 }).toList
      individualExtractors(0).toString mustEqual myIndividualExtractors(0)._2.toString
      individualExtractors(1).toString mustEqual myIndividualExtractors(1)._2.toString
      individualExtractors(2).toString mustEqual myIndividualExtractors(2)._2.toString
      individualExtractors(3).toString mustEqual myIndividualExtractors(3)._2.toString
    }

  }
}