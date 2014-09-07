package lv.ddgatve.screenscrapers.csv

import java.io.PrintWriter

object CSVWriter {

  val workingDir = "/home/kalvis/workspace/ddgatve-screenscrapers/src/main/resources/data/"
  val individualsNames = List("Valid", "Modified", "Unmodified", "District", "Saeima", "Party")
  val tableColumns = List("PostNum", "Candidate",
    "PreNum", "Pluses",
    "Minuses", "Points",
    "Result", "Saeima",
    "Party", "District",
    "BallotsForParty")

  val Pattern = """(\S+)""".r

  def concatenate(strings: List[String]) = strings map { x =>
    x match {
      case Pattern(c) => x
      case _ => "\"" + x + "\""
    }

  } mkString ","

  def write(fname: String, columns: List[String], values: List[List[String]]): Unit = {
    Some(new PrintWriter(fname)).
      foreach {
        p =>
          {
            p.write(concatenate(columns))
            p.write(System.getProperty("line.separator"))
            for (row <- values) {
              p.write(concatenate(row))
              p.write(System.getProperty("line.separator"))
            }
          }
          p.close
      }
  }

  def writeDatasets(saeima: Int, stTables: Map[String, List[List[String]]]): Unit = {
    CSVWriter.write(workingDir + f"singleCols$saeima%02d.csv",
      individualsNames,
      stTables.get("singleCols").get)
    CSVWriter.write(workingDir + f"candidates$saeima%02d.csv",
      tableColumns,
      stTables.get("candidates").get)
  }

}