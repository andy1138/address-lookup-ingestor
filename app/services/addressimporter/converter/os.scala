/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services.addressimporter.converter

object OSCleanup {

  val Uprn_Idx = 3

  def removeBannedStreets(s: String): String = {
    val wordList = List[String](
      "From ", "Pump ", "Pumping ", "Mast ", "Hydraulic Ram", "Helipad ", "Across From", "Fire Station",
      "Awaiting Conversion", "Ppg Sta", "Footway", "Bridge", "Pipeline", "Redevelopment"
    )
    if (wordList.exists(w => s.toLowerCase.contains(w.toLowerCase))) "" else s
  }


  implicit class StringCleanup(s: String) {
    def cleanup: String = {
      val s1 = s.trim
      if (s1 == "\"\"") ""
      else {
        val s2 = if (s1.nonEmpty && s1.head == '"') s1.tail.trim else s1
        if (s2.nonEmpty && s2.last == '"') s2.init.trim else s2
      }
    }

    def rmDupSpace: String = {
      s.replaceAll("  ", " ")
    }

    def capitalisation: String = {
      Capitalisation.normaliseAddressLine(s)
    }
  }

}


object OSCsv {
//  var csvFormat = 2

  val RecordIdentifier_idx = 0
}


object OSHeader {
  val RecordId = "10"

  val Version_Idx = 7
}


object OSBlpu {
  val RecordId = "21"

//  val V1LogicalStatusIdx = 4
//  val V1PostalAddrCodeIdx = 16
//  val V1PostcodeIdx = 17

  val LogicalStatusIdx = 4
  val PostalAddrCodeIdx = 19
  val PostcodeIdx = 20


  def isSmallPostcode(csv: Array[String]): Boolean = {
//    println("isSmallPostcode length:" + csv.length + "   "  + csv.mkString(","))

    csv(PostalAddrCodeIdx) != "N"
  }

  def apply(csv: Array[String]):OSBlpu = new OSBlpu(csv.toVector)
  import OSCleanup._

//  def apply(csv: Array[String]): OSBlpu =
//      OSBlpu(csv(Uprn_Idx).toLong, csv(V2LogicalStatusIdx).head, csv(V2PostcodeIdx))
}

class OSBlpu(csv: Vector[String]) {
  import OSCleanup._
  import OSBlpu._

  lazy val uprn = csv(Uprn_Idx).toLong

  def logicalStatus:Char = csv(LogicalStatusIdx).head

  def postcode:String = csv(PostcodeIdx)
}
//case class OSBlpu(uprn: Long, logicalStatus: Char, postcode: String)


object OSDpa {
  val RecordId = "28"

  import OSCleanup._

  val SubBuildingNameIdx = 7
  val BuildingNameIdx = 8
  val BuildingNumberIdx = 9
  val DependentThoroughfareNameIdx = 10
  val ThoroughfareNameIdx = 11
  val DoubleDependentLocalityIdx = 12
  val DependentLocalityIdx = 13
  val PostTownIdx = 14
  val PostcodeIdx = 15


//  def apply(csv: Array[String]): OSDpa =
//      OSDpa(
//        csv(Uprn_Idx).toLong,
//        csv(V2SubBuildingNameIdx).cleanup,
//        csv(V2BuildingNameIdx).cleanup,
//        csv(V2BuildingNumberIdx).cleanup,
//        csv(V2DependentThoroughfareNameIdx).cleanup,
//        csv(V2ThoroughfareNameIdx).cleanup,
//        csv(V2DoubleDependentLocalityIdx).cleanup,
//        csv(V2DependentLocalityIdx).cleanup,
//        csv(V2PostTownIdx).cleanup,
//        csv(V2PostcodeIdx).cleanup)

  def apply(csv: Array[String]):OSDpa = new OSDpa(csv.toVector)
}

class OSDpa(csv: Vector[String]) {

  import OSCleanup._
  import OSDpa._

  lazy val uprn:Long = csv(Uprn_Idx).toLong

  def subBuildingName:String = csv(SubBuildingNameIdx).cleanup

  def buildingName:String = csv(BuildingNameIdx).cleanup

  def buildingNumber:String = csv(BuildingNumberIdx).cleanup

  def dependentThoroughfareName:String = csv(DependentThoroughfareNameIdx).cleanup

  def thoroughfareName:String = csv(ThoroughfareNameIdx).cleanup

  def doubleDependentLocality:String = csv(DoubleDependentLocalityIdx).cleanup

  def dependentLocality:String = csv(DependentLocalityIdx).cleanup

  def postTown:String = csv(PostTownIdx).cleanup

  def postcode:String = csv(PostcodeIdx).cleanup

}
//case class OSDpa(uprn: Long, subBuildingName: String, buildingName: String, buildingNumber: String,
//                 dependentThoroughfareName: String, thoroughfareName: String, doubleDependentLocality: String,
//                 dependentLocality: String, postTown: String, postcode: String)


object OSStreet {
  val RecordId = "11"

  import OSCleanup._

  val RecordTypeIdx = 4

//  def apply(csv: Array[String]): OSStreet = OSStreet(csv(Uprn_Idx).toLong, csv(RecordTypeIdx).head)
  def apply(csv: Array[String]):OSStreet = new OSStreet(csv.toVector)
}

class OSStreet(csv: Vector[String]) {
  import OSCleanup._
  import OSStreet._

  lazy val usrn:Long = csv(Uprn_Idx).toLong

  def recordType:Char = csv(RecordTypeIdx).head
}


object OSStreetDescriptor {
  val RecordId = "15"

  import OSCleanup._

  val DescriptionIdx = 4
  val LocalityIdx = 5
  val TownIdx = 6
  val LanguageIdx = 8

  def isEnglish(csv: Array[String]): Boolean = csv(LanguageIdx) == "ENG"

//  def apply(csv: Array[String]): OSStreetDescriptor = OSStreetDescriptor(
//    csv(Uprn_Idx).toLong,
//    csv(DescriptionIdx).cleanup,
//    csv(LocalityIdx).cleanup,
//    csv(TownIdx).cleanup.intern,
//    csv(LanguageIdx).cleanup.intern)
  def apply(csv: Array[String]):OSStreetDescriptor = new OSStreetDescriptor(csv.toVector)

}

//case class OSStreetDescriptor(usrn: Long, description: String, locality: String, town: String, language: String)
class OSStreetDescriptor(csv: Vector[String]) {

  import OSCleanup._
  import OSStreetDescriptor._

  lazy val usrn:Long = csv(Uprn_Idx).toLong

  def description: String = csv(DescriptionIdx).cleanup

  def locality: String = csv(LocalityIdx).cleanup

  def town: String = csv(TownIdx).cleanup.intern
}


object OSLpi {
  val RecordId = "24"

  import OSCleanup._

  val LogicalStatusIdx = 6
  val SaoStartNumberIdx = 11
  val SaoStartSuffixIdx = 12
  val SaoEndNumberIdx = 13
  val SaoEndSuffixIdx = 14
  val SaoTextIdx = 15
  val PaoStartNumberIdx = 16
  val PaoStartSuffixIdx = 17
  val PaoEndNumberIdx = 18
  val PaoEndSuffixIdx = 19
  val PaoTextIdx = 20
  val UsrnIdx = 21


  def apply(csv: Array[String]): OSLpi = new OSLpi(csv.toVector)}

//case class OSLpi(uprn: Long, logicalStatus: Char, saoStartNumber: String, saoStartSuffix: String,
//                 saoEndNumber: String, saoEndSuffix: String, saoText: String,
//                 paoStartNumber: String, paoStartSuffix: String, paoEndNumber: String,
//                 paoEndSuffix: String, paoText: String, usrn: Long)

class OSLpi(csv: Vector[String]) {

  import OSCleanup._
  import OSLpi._

  lazy val uprn:Long = csv(Uprn_Idx).toLong

  def logicalStatus:Char = csv(LogicalStatusIdx).head

  def saoStartNumber:String = csv(SaoStartNumberIdx).cleanup

  def saoStartSuffix:String = csv(SaoStartSuffixIdx).cleanup

  def saoEndNumber:String = csv(SaoEndNumberIdx).cleanup

  def saoEndSuffix:String = csv(SaoEndSuffixIdx).cleanup

  def saoText:String = csv(SaoTextIdx).cleanup

  def paoStartNumber:String = csv(PaoStartNumberIdx).cleanup

  def paoStartSuffix:String = csv(PaoStartSuffixIdx).cleanup

  def paoEndNumber:String = csv(PaoEndNumberIdx).cleanup

  def paoEndSuffix:String = csv(PaoEndSuffixIdx).cleanup

  def paoText:String = csv(PaoTextIdx).cleanup

  def usrn:Long = csv(UsrnIdx).toLong
}

