package RandomEngine

import java.net.{SocketTimeoutException, UnknownHostException}

import Logging.Logger

import scalaj.http._

/**
  * TrueRandomSource.scala
  *
  * This class is a client for communicating to random.org for true random bits.
  */
object TrueRandomSource {
  // MARK: Fields
  var numberOfRandomBitsRemaining = 0 // Number of bits remaining
  var randomBitsString = "" // Buffer for storing the bits

  def initialize(): Unit = {
    // On initialization, get 2,000 random bits.
    acquireMoreRandomBitsFromServer(2000)
  }

  private def acquireMoreRandomBitsFromServer(numberOfBits: Int): Unit = {
    // Calculate number of bits rounded down to nearest 8. Get a minimum of 120 bits per query
    val bitsRounded = math.max(120, (numberOfBits / 8) * 8)

    try {
      // Interact with Random.org's Plaintext API.
      val result = Http("https://www.random.org/cgi-bin/randbyte?nbytes=" + (bitsRounded / 8) + "&format=b")
        .header("Content-Type", "text/plain")
        .header("Charset", "UTF-8")
        .option(HttpOptions.readTimeout(10000))

      if (result.asString.isError) {
        Logger.error("An error occurred trying to get random data.", this)
      }

      else {
        // Update bit reservoir string and number of bits remaining
        randomBitsString = randomBitsString.concat(result.asString.body.filter(_ >= ' ').replaceAll(" ", ""))
        numberOfRandomBitsRemaining = randomBitsString.length
      }
    }
    catch {
      case unex: UnknownHostException => Logger.warn("RandomManager", "Could not connect to random.org")
      case toex: SocketTimeoutException => Logger.warn("RandomManager", "Could not reach host random.org")
    }
  }

  def requestRandomBits(numberOfBits: Int): String = {
    if (numberOfRandomBitsRemaining < numberOfBits) { // If there aren't enough bits remaining in reservoir, grab more.
      Logger.info("RandomManager", "We only have " + numberOfRandomBitsRemaining + " bits (" + numberOfBits + " requested).")
      Logger.info("RandomManager", "Acquiring " + (numberOfBits * 2) + " more bits.")
      acquireMoreRandomBitsFromServer(numberOfBits * 2) // Get them from Random.org (2x as many requested)
    }

    val str = randomBitsString.take(numberOfBits) // Get first n-bits of the string
    randomBitsString = randomBitsString.drop(numberOfBits) // Remove them from the reservoir string.
    numberOfRandomBitsRemaining = randomBitsString.length // Update number of bits remaining

    str // Return value
  }
}
