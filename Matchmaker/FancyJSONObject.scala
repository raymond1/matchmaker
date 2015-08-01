package Matchmaker


import org.json._

/**
 * @author default
 * 
 * A wrapper class to allow sorting of JSON listings
 */
class FancyJSONObject(_jsonObject: JSONObject, _sanitizedString: String){
  val sortingString = _sanitizedString
  val jsonObject = _jsonObject
  def compareTo(a:FancyJSONObject, b:FancyJSONObject) = {
    val comparisonValue = a.sortingString.compareToIgnoreCase(b.sortingString) 
    if (comparisonValue < 0){
      -1
    }else if (comparisonValue == 0){
      0
    }
    else{
      1
    }
  }
}
