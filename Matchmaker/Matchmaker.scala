package Matchmaker

import java.io._
import java.nio.file._
import java.nio.file.Paths
import org.json._
import scala.io.Source
import scala.collection.immutable.TreeSet

/**
 * @author default
 */
object Matchmaker{
  def main(args: Array[String]) {
    //sortedListings is a TreeSet[FancyJSONObject]
    val sortedListings = getSortedListings("listings.txt")

    var outputString = ""

    //get a list of products from file    
    val unsortedProducts = getProducts("products.txt")

    //iterate through all the unsorted products, and for each unsorted product,
    //find the listings that match with it. Put all the matches into a collection
    //with an iterator called matches.
    val matches = getMatches(unsortedProducts, sortedListings)

    for (singleMatch <- matches){
      var listingString = ""
      for (listing <- singleMatch.listings){
        listingString += listing.jsonObject.toString() + ","
      }
      listingString = listingString.substring(0, listingString.length() - 1)
      
      
      outputString += "{\"product_name\":\"" + singleMatch.product.jsonObject.getString("product_name") + "\",\"listings\":[" + listingString + "]}\n"
    }
    
    val pw = new PrintWriter(new File("results.txt" ))
    pw.write(outputString)
    pw.close
  }
  
  def getMatches(unsortedProducts: Iterator[FancyJSONProduct], sortedListings: TreeSet[FancyJSONListing]): List[Match] = {
    var matches = List.empty[Match]
    for (unsortedProduct <- unsortedProducts){
      val matchingListings = getListingsMatchingSingleProduct(unsortedProduct, sortedListings)
      if (!matchingListings.isEmpty){
        matches = matches :+ new Match(unsortedProduct, matchingListings)//new List() // matchingListings //new Match(unsortedProduct,matchingListings)
      }
    }
    
    return matches
  }
    
  def getProducts(filename:String): Iterator[FancyJSONProduct]= {
    val jsonObjects = getJSONObjectsFromFile(filename)
    val fancyJSONProducts = for (jsonObject <- jsonObjects) yield{
      new FancyJSONProduct(jsonObject, getSanitizedString(jsonObject.getString("product_name")))
    }
    fancyJSONProducts
  }
  
  def getSortedListings(filename:String):TreeSet[FancyJSONListing] = {
    //get a list of unsorted listings from file
    val unsortedListings = getFancyJSONListings(filename)
    
    //sort the listings
    var sortedListings = TreeSet.empty(FancyJSONListingOrdering)
    
    for (unsortedListing <- unsortedListings){
      sortedListings += unsortedListing
    }
    
    sortedListings
  }
  
  //fancy means a JSON object that has a sortable field
  def getFancyJSONListings(filename:String):Iterator[FancyJSONListing] = {
    val jsonObjects = getJSONObjectsFromFile(filename)
    val fancyJSONListings = for (jsonObject <- jsonObjects) yield{
      new FancyJSONListing(jsonObject, getSanitizedString(jsonObject.getString("title")))
    }
    fancyJSONListings
  }
  
  def getSanitizedString(inputString:String):String = { 
    val lowercase = inputString.toLowerCase()
    val noFunnyCharactersPattern = "[^a-z0-9]".r
    
    
    val standardCharactersProductName = noFunnyCharactersPattern.replaceAllIn(lowercase, " ")
    //Then, compress all contiguous whitespaces into a single space
    val largeWhitespaces = "\\s{2,}".r
    
    val standardizedWhitespaceSize = largeWhitespaces.replaceAllIn(standardCharactersProductName, " ")
    
    val trimmed = standardizedWhitespaceSize.trim
    
    trimmed
  }
  
  def getFancyJSONObjectsFromFile(filename: String): Iterator[FancyJSONObject] = {
    var normalJSONObjects = getJSONObjectsFromFile(filename)
    var fancyJSONObjects = for (normalJSONObject <- normalJSONObjects)
    yield new FancyJSONObject(normalJSONObject, "")
    
    fancyJSONObjects
  }
  
  def getJSONObjectsFromFile(filename: String): Iterator[JSONObject] = {
    val JSONObjects = for (line <- Source.fromFile(filename).getLines()) yield {
      val jsonTokener = new org.json.JSONTokener(line)
      val jsonObject = new org.json.JSONObject(jsonTokener)
      jsonObject
    }
    JSONObjects
  }

  //returns a closure that can be used as a filter to find all members of a TreeSet[FancyJSONListing]
  //that have the same sortingString as the product parameter
  def getMatchingFunction(product:FancyJSONProduct): FancyJSONListing => Boolean = {
    val _product = product
    def predicate(fancyJSONListing:FancyJSONListing): Boolean = {
      if (product.sortingString == fancyJSONListing.sortingString){
        true
      }
      else{
        false
      }
    }
    predicate
  }
  
  //Takes in a product and a sorted tree of listings and returns all of the listings that match
  //with the product.
  def getListingsMatchingSingleProduct(product: FancyJSONProduct, listings: TreeSet[FancyJSONListing]): List[FancyJSONListing] = {
    //search for FancyJSONObject in listings
    
    val matchingFunction = getMatchingFunction(product)
    val matchingListings = listings.filter(matchingFunction).toList
    matchingListings

  }
  
  def trimLeft(str: String) = str.dropWhile(_.isWhitespace)
}

object FancyJSONObjectOrdering extends Ordering[FancyJSONObject]{
  def compare(a:FancyJSONObject, b:FancyJSONObject) = {
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

object FancyJSONListingOrdering extends Ordering[FancyJSONListing]{
  def compare(a:FancyJSONListing, b:FancyJSONListing) = {
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
