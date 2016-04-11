package MorphingEngine

import Logging.Logger

/**
  * MorphingKeyManager.scala
  * Contains the morphing functions available to both the client and the server. The client and server may be running
  * different versions of QuantumSim, so checks for which is available is imperative.
  */
object MorphingKeyManager {
  var fx: List[MorphingFunction] = List() // List of hashing functions.
  var messagesPerMorph = 2 // Default number of messages per morph!

  /*
   * initialize() -> Initialises the MorphingManager by adding the functions to the list (fx).
   */
  def initialize(): Unit = {
    fx = fx.::(new MorphingFunction("MD5", 10))     // Add that we have MD5 installed with Strength 10.
    fx = fx.::(new MorphingFunction("SHA512", 70))  // Add that we have SHA-512 installed with Strength 70.
    fx = fx.::(new MorphingFunction("SHA256", 50))  // Add that we have SHA-256 installed with Strength 50.

    fx = fx.sortWith(_.security > _.security) // Sort by security
  }

  /*
   * getSHA512() -> Gets the SHA-512 hash of an input string.
   * @input s: Input string for the hash to be computed of.
   * @output: SHA-512 hash of the input, s.
   */
  def getSHA512(s: String): String = {
    // Besides "MD5", "SHA-256", and other hashes are available
    val m = java.security.MessageDigest.getInstance("SHA-512").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }


  /*
   * getSHA256() -> Gets the SHA-256 hash of an input string.
   * @input s: Input string for the hash to be computed of.
   * @output: SHA-256 hash of the input, s.
   */
  def getSHA256(s: String): String = {
    // Besides "MD5", "SHA-256", and other hashes are available
    val m = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }

  /*
   * getMD5() -> Gets the MD5 hash of an input string. (Not recommended. Collision attacks exist)
   * @input s: Input string for the hash to be computed of.
   * @output: MD5 hash of the input, s.
   */
  def getMD5(s: String): String = {
    val m = java.security.MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8"))
    Logger.warn("MD5 is considered insecure, you should seek a better morphing function.", this)
    m.map("%02x".format(_)).mkString
  }

  /*
   * getHashingFunctions() -> Method for retrieving the list of available functions in this version of QuantumSim.
   * @output: List of functions available in String form
   */
  def getHashingFunctions: String = {
    var listOfFunctions = ""
    for (h <- fx)
      listOfFunctions += (h.name + ",")

    listOfFunctions.substring(0, listOfFunctions.length - 1)
  }
}
