package lv.ddgatve.screenscrapers.elections

import org.specs2.mutable.Specification

class CSVRefactorSpec extends Specification {

  "CSVRefactor candidate Name, Surname, Sex" should {
    val r = new CSVRefactor(6)
    val arg1 = "Manfrēds Leontijs Šneps-Šnepe"
    val arg2 = "Maija Nora Tabaka"
    "Divide correctly 'Manfrēds Leontijs Šneps-Šnepe'" in {
      val name = r.getCandidateName(arg1)
      val surname = r.getCandidateSurname(arg1)
      name mustEqual "Manfrēds Leontijs"
      surname mustEqual "Šneps-Šnepe"
    }
    "Recognize 'Manfrēds Leontijs Šneps-Šnepe'" in {
      val result = r.getCandidateSex(arg1)
      result mustEqual "Male"
    }
    "Analyze 'Maija Nora Tabaka'" in {
      val name = r.getCandidateName(arg2)
      val surname = r.getCandidateSurname(arg2)
      name mustEqual "Maija Nora"
      surname mustEqual "Tabaka"
    }
  }

  "CSVRefactor" should {
    "output saeima06" in {
      val r = new CSVRefactor(6)
      val result = r.refactorCandidateCvs
      result mustEqual 1765
    }
    "output saeima07" in {
      val r = new CSVRefactor(7)
      val result = r.refactorCandidateCvs
      result mustEqual 1958
    }
    "output saeima08" in {
      val r = new CSVRefactor(8)
      val result = r.refactorCandidateCvs
      result mustEqual 1897
    }
    "output saeima09" in {
      val r = new CSVRefactor(9)
      val result = r.refactorCandidateCvs
      result mustEqual 1954
    }
    "output saeima10" in {
      val r = new CSVRefactor(10)
      val result = r.refactorCandidateCvs
      result mustEqual 1235
    }
    "output saeima11" in {
      val r = new CSVRefactor(11)
      val result = r.refactorCandidateCvs
      result mustEqual 1092
    }
  }

  "CSVRefactor object" should {
    "createCandidatesAll" in {
      val result = CSVRefactor.createCandidatesAll
      result mustEqual 9901
    }
  }
}