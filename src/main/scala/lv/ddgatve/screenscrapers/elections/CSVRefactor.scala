package lv.ddgatve.screenscrapers.elections

import lv.ddgatve.screenscrapers.csv.CSVReader
import org.apache.commons.csv.CSVRecord
import lv.ddgatve.screenscrapers.csv.CSVWriter

object CSVRefactor {
  val outFields = List("PostNum", "PreNum", "CandidateName", "CandidateSurname",
    "Sex", "Saeima", "PartyNum", "PartyAbbr", "DistrNum", "DistrName",
    "BallotsForParty", "Pluses", "Minuses", "Points", "Result")
  def createCandidatesAll(): Int = {

    val outFile = f"src/main/resources/data-candidates/candidates-all.csv"

    val inFiles = ((6 to 11) map (x =>
      f"src/main/resources/data-candidates/candidates$x%02d.csv")).toList

    val lineSets = for (inFile <- inFiles) yield {
      CSVReader.readRecords(inFile)
    }
    val result = lineSets.flatten map (x => {
      outFields map (f => {
        x.get(f)
      })
    })
    CSVWriter.write(outFile, outFields, result)
    result.size
  }
}

class CSVRefactor(saeima: Int) {

  val inFields = if (saeima == 6) {
    List("PostNum", "Kandidats", "PreNum",
      "Plusi", "Svitrojumi", "Punkti", "Rezultats",
      "Saeima", "Partija", "PartijaNum", "Apgabals", "Balsis")
  } else {
    List("PostNum", "Candidate", "PreNum",
      "Pluses", "Minuses", "Points", "Result",
      "Saeima", "Party", "District", "BallotsForParty")
  }

  val outFields = List("PostNum", "PreNum", "CandidateName", "CandidateSurname",
    "Sex", "Saeima", "PartyNum", "PartyAbbr", "DistrNum", "DistrName",
    "BallotsForParty", "Pluses", "Minuses", "Points", "Result")

  val partyRecords = ((6 to 11) map (x =>
    (x, CSVReader.readRecords(f"src/main/resources/data-parties/saeima$x%02d.csv")))).toMap

  val distrNums = Map("Rīga" -> "1",
    "Vidzeme" -> "2",
    "Latgale" -> "3",
    "Kurzeme" -> "4",
    "Zemgale" -> "5")
  val distrNames = Map("1" -> "Rīga",
    "2" -> "Vidzeme",
    "3" -> "Latgale",
    "4" -> "Kurzeme",
    "5" -> "Zemgale")

  def getCandidateName(s: String): String = {
    val sep = s.lastIndexOf(" ")
    return s.substring(0, sep)
  }

  def getCandidateSurname(s: String): String = {
    val sep = s.lastIndexOf(" ")
    return s.substring(sep + 1)
  }

  def getCandidateSex(s: String): String = {
    val name = getCandidateName(s)
    val surname = getCandidateSurname(s)
    val maleNames = List("Agris", "Aigars", "Aivars", "Aivars Mārtiņš",
      "Aldis", "Aldonis", "Aleksandrs", "Aleksejs", "Alvils",
      "Anatolijs", "Andrejs", "Andris", "Andrējs", "Anrī", "Antonijs", "Antons",
      "Āris", "Armands", "Arnis", "Arno", "Arnolds",
      "Artis", "Arturs", "Artūrs", "Arvīds", "Arvo", "Atis",
      "Bruno",
      "Dāgs", "Dāvis", "Deniss", "Druvis", "Dzintars",
      "Edgars", "Edmunds", "Edvīns", "Egīls", "Egons", "Einars", "Eižens", "Elmārs", "Ēriks", "Ernesto", "Ervīns",
      "Fricis",
      "Gatis", "Georgijs", "Gints", "Grigorijs", "Gunārs", "Gundars", "Guntars", "Guntars Agate", "Guntis", "Gvido",
      "Ģedimins", "Ģirts",
      "Heinis", "Heino", "Henrijs", "Hosams", "Hosams Abu",
      "Igors", "Ilgars", "Ilgvars", "Imants", "Imants Roberts", "Indulis", "Ingmārs", "Ilmārs", "Iļja", "Ivars", "Ivo", "Ivs",
      "Jānis", "Jāzeps", "Jevgenijs", "Jevgēnijs", "Jevgeņijs",
      "Ju-liņ", "Jurijs", "Juris", "Juris Guntis", "Juris Rūdolfs",
      "Kārlis", "Kārlis Jūlijs", "Konstantīns", "Kristaps",
      "Laimonis", "Leonīds", "Leons", "Leopolds",
      "Manfrēds Leontijs", "Māris", "Māris Krists", "Mārtiņš", "Mārtiņš Gunārs",
      "Mečislavs", "Medarts", "Mihails", "Modris",
      "Normunds",
      "Ņikita",
      "Odisejs", "Ojārs", "Orests", "Orlando", "Oskars",
      "Pāvels", "Pēteris",
      "Rafi", "Raimonds", "Raitis", "Raivo", "Reinholds Arnolds", "Renārs", "Roberts", "Rolands", "Romalds",
      "Sandis", "Sergejs", "Sergo", "Sigurds", "Staņislavs",
      "Toms",
      "Uldis", "Uldis-Ivars",
      "Vadims", "Valdis", "Valentīns", "Valērijs", "Valfrīds", "Valters", "Večeslavs", "Veiko", "Ventis",
      "Viesturs", "Vigo", "Viktors", "Vilis", "Vilmārs", "Vilmārs Stanislavs", "Vilnis", "Visvaldis", "Vitālijs", "Vitauts",
      "Vjačeslavs", "Vladimirs", "Vladislavs",
      "Ziedonis", "Zigmārs", "Zigurds")
    val femaleNames = List("Andra", "Anita", "Anna", "Antoņina", "Anžela", "Astrīda", "Ausma",
      "Dace", "Daiga", "Daina", "Dite", "Dzintra",
      "Elīna", "Evisa",
      "Ilze", "Ināra", "Inese", "Inga", "Inta", "Irēna", "Irina",
      "Leonarda", "Lidija", "Lilita", "Līvija",
      "Maija", "Maija Nora", "Māra", "Marija",
      "Nataļja", "Nazira",
      "Ņina",
      "Olga",
      "Ruta",
      "Svetlana",
      "Tatjana",
      "Valda", "Valentina", "Valentīna", "Vija", "Viola", "Violetta")
    if ((name.endsWith("s") || name.endsWith("š")) &&
      (surname.endsWith("s") || surname.endsWith("š"))) {
      "Male"
    } else if ((name.endsWith("a") || name.endsWith("e")) &&
      (surname.endsWith("a") || surname.endsWith("e"))) {
      "Female"
    } else if (maleNames.contains(name)) {
      "Male"
    } else if (femaleNames.contains(name)) {
      "Female"
    } else {
      println("WARNING: Could not identify candidate sex: '" + s + "'")
      "Other"
    }
  }

  def getPartyAbbr(saeima: Int, partyNum: Int): String = {
    val recordSet = partyRecords.get(saeima).get
    val recordLine = recordSet(partyNum - 1)
    val abbr = recordLine.get("Saīsinājums")
    abbr
  }

  def fixName(arg: String): String = {
    arg match {
      case "Mārtiņš Gunārs Bauze - Krastiņš" => "Mārtiņš Gunārs Bauze-Krastiņš"
      case "Mārtiņš Gunārs Bauze-Krasti" => "Mārtiņš Gunārs Bauze-Krastiņš"
      case "Sanita Pavļuta - Deslande" => "Sanita Pavļuta-Deslande"
      case "Jānis Kleinbergs - Vītols" => "Jānis Kleinbergs-Vītols"
      case "Andra Austere - Maspāne" => "Andra Austere-Maspāne"
      case _ => arg
    }
  }

  def recordToMap(arg: CSVRecord): Map[String, String] = {
    val candidate = fixName(if (saeima == 6) arg.get("Kandidats").trim()
    else arg.get("Candidate").trim())
    val candidateName = getCandidateName(candidate)
    val candidateSurname = getCandidateSurname(candidate)
    val candidateSex = getCandidateSex(candidate)
    val partyNum = if (saeima == 6) arg.get("PartijaNum")
    else arg.get("Party")
    val partyAbbr = getPartyAbbr(saeima, partyNum.toInt)
    val distrNum = if (saeima == 6) distrNums.get(arg.get("Apgabals")).get
    else arg.get("District")
    val distrName = if (saeima == 6) {
      val fromTable = arg.get("Apgabals")
      val fromNumber = distrNames.get(distrNum).get
      if (fromTable != fromNumber) {
        println("WARNING: DistrName mismatch for saeima '" +
          saeima + "': " + fromTable + " vs " + fromNumber)
      }
      fromTable
    } else distrNames.get(arg.get("District")).get

    val ballotsForParty = if (saeima == 6) {
      arg.get("Balsis")
    } else { arg.get("BallotsForParty") }

    val pluses = if (saeima == 6) arg.get("Plusi") else arg.get("Pluses")
    val minuses = if (saeima == 6) arg.get("Svitrojumi") else arg.get("Minuses")
    val points = if (saeima == 6) arg.get("Punkti") else arg.get("Points")
    val result = if (saeima == 6) arg.get("Rezultats").trim() else arg.get("Result").trim()

    val theResult = outFields map (outField => {
      outField match {
        case "PostNum" => (outField, arg.get("PostNum"))
        case "PreNum" => (outField, arg.get("PreNum"))
        case "CandidateName" => (outField, candidateName)
        case "CandidateSurname" => (outField, candidateSurname)
        case "Sex" => (outField, candidateSex)
        case "Saeima" => (outField, arg.get("Saeima"))
        case "PartyNum" => (outField, partyNum)
        case "PartyAbbr" => (outField, partyAbbr)
        case "DistrNum" => (outField, distrNum)
        case "DistrName" => (outField, distrName)
        case "BallotsForParty" => (outField, ballotsForParty)
        case "Pluses" => (outField, pluses)
        case "Minuses" => (outField, minuses)
        case "Points" => (outField, points)
        case "Result" => (outField, result)
        case _ => {
          println("WARNING: Unexpected field: " + outField)
          (outField, "")
        }
      }
    })
    theResult.toMap
  }

  def refactorCandidateCvs(): Int = {
    val outFile = f"src/main/resources/data-candidates/candidates$saeima%02d.csv"
    val inFiles = if (saeima == 6) {
      ((1 to 19) map (x =>
        f"src/main/resources/data-saeima06/saeima06.$x%d.csv")).toList
    } else {
      List(f"src/main/resources/data/candidates$saeima%02d.csv")
    }

    val lineSets = for (inFile <- inFiles) yield {
      val records = CSVReader.readRecords(inFile)
      records map recordToMap
    }
    val result = lineSets.flatten map (x => {
      outFields map (f => {
        x.get(f).get
      })
    })
    CSVWriter.write(outFile, outFields, result)
    result.size
  }

}