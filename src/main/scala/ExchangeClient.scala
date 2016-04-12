import java.io.PrintStream
import java.net.{InetAddress, Socket}

import Logging.Logger
import Messages.{ClassicalMessage, QuantumMessage}
import RandomEngine.{TrueRandomSource => TRandom}

import scala.io.BufferedSource
import scala.util.control.Breaks._

/**
  * Created by annie on 18/03/2016.
  */
class ExchangeClient (ip: String, port: Int, useMK: Boolean, randomMK: Boolean, useRP: Boolean) {
  var enableMorphingKey = useMK     // If we want to use Morphing Encryption Keys (experimental)
  val randomMorphMode = randomMK    // If we want to use the random morphing mode (experimental)
  val rotaryPolarisation = useRP    // If we want to rotationally polarise our 'quantum channel' (experimental)

  val partner = new Principal(ip + ":" + port)

  /*
   * executeKeyExchange() -> This function acts as 'Grand Central Dispatch' for executing the key exchange
   *                         with a partner principal.
   */
  def executeKeyExchange(): Unit = {
    // Display warnings and information if needed
    if (partner.secure_communications_ready)
      Logger.info("Key Exchange has already been completed with this server.", this)
    if (enableMorphingKey)
      Logger.warn("Mutating Key Communications are enabled (experimental feature!)", this)
    if (randomMorphMode)
      Logger.warn("Mutations will be at random. This is an experimental feature!", this)
    if (rotaryPolarisation)
      Logger.warn("Quantum Keying Material will be exchanged over a rotationally polarised channel (experimental).", this)

    // PHASE 1: Prompt user for keyphrase.
    val keyphrase = promptKeyphraseFromUser()
    partner.key_bits = keyphrase

    // PHASE 2: Request initialisation with server.
    val accepted = requestServerInitialisation()
    if (!accepted)
      return

    // PHASE 2.5: Rotationally Polarise Channel
    if (rotaryPolarisation) {
      val rotaryPolarisationSuccessful = exchangeRotationPolarisation()

      if (!rotaryPolarisationSuccessful)
        return
    }
    // If there is a pre-shared secret key for use, use that key to encrypt a message to share a rotation degree
    // If not, Logger.warn it, but continue regardless.

    // PHASE 3: Send quantum bits to the server.
    val completedKeyBitsSuccessfully = sendQuantumBits()
    if (!completedKeyBitsSuccessfully)
      return

    // PHASE 4: Publicly share basis bits using classical channel.
    val completedBasisBitsSuccessfully = shareBasisBits()
    if (!completedBasisBitsSuccessfully)
      return

    // PHASE 5: Compute the Final Shared Key
    val finalKeyExchanged = computeFinalKey()
    if (finalKeyExchanged == "")
      return

    // PHASE E1: Agree to use Morphing Key together...
    if (enableMorphingKey) {
      val morphingKeyAgreed = agreeMorphingKeyMode()
      if (morphingKeyAgreed == "error") {
        return
      }
      else if (morphingKeyAgreed == "none") {
        Logger.warn("Mutating Engine Disabled. See above entries for more details.", this)
      }
    }

    // PHASE 6: Send the ready signal to signify the end of the modified BB84 Protocol
    val completedReadySignal = sendReadySignal()
    if (!completedReadySignal)
      return

    // COMPLETED KEY EXCHANGE: We are done!
    partner.secure_communications_ready = true

    Logger.info("All key exchange completed! Ready to send secure messages!", this)

    Utilities.createInfoMessageBox(
      "Key exchanged successfully!",
      "Successfully exchanged a key with client " + ip + ":" + port + "\nThe final key is: " + partner.final_key
    )
  }

  /*
   * promptKeyphraseFromUser() -> PHASE 1: This function requests the user to input their chosen key phrase.
   * @output: String containing the binary string of the user's chosen key phrase.
   */
  def promptKeyphraseFromUser(): String = {
    Logger.info("[INPUT REQUESTED] Requesting input from user for passphrase", this)

    val input = scala.swing.Dialog.showInput(null, "Enter passphrase (20+ chars) or 'random' for a random passphrase", initial = "random")
    var phrase = "random"

    input match {
      case Some(s) =>
        partner.key_string = s
        phrase = s
      case None =>
    }


    var keyBits: String = { // Converts the key into a binary string representation.
      if (phrase != "random")
        Utilities.stringToBinaryString(phrase)
      else
        TRandom.requestRandomBits(300) // If random was selected, get 300 random bits.
    }

    // Check length of binary string
    if (keyBits.length < 300) {
      Logger.warn("Your passphrase is too short, we are appending " + (300 - keyBits.length) + " extra random bits", this)
      while (keyBits.length < 300)
        keyBits += TRandom.requestRandomBits(2)
    }

    keyBits
  }

  /*
   * requestServerInitialisation() -> PHASE 2: This function checks with the server if it is accepting key exchanges!
   * @output: Boolean containing whether or not we can proceed!
   */
  def requestServerInitialisation(): Boolean = {
    val requestMessage = new ClassicalMessage(ClassicalMessage.KEY_EXCHANGE_INIT, "key_size:" + BB84Simulator.AES_KEYSIZE)

    val responseMessage = sendClassicalMessage(requestMessage, waitForResponse = true)

    responseMessage.symbol match {
      case ClassicalMessage.KEY_EXCHANGE_ACCEPT =>
        Utilities.createInfoMessageBox(
          "Server accepted request",
          "Server " + ip + ":" + port + " has accepted the key exchange request!"
        )
        true
      case ClassicalMessage.KEY_EXCHANGE_DENIED =>
        Utilities.createInfoMessageBox(
          "Server denied request",
          "Server " + ip + ":" + port + " has denied the key exchange request!"
        )
        Logger.error("Cannot continue with key exchange. Server " + ip + ":" + port + " denied key exchange!", this)
        false
      case ClassicalMessage.KEY_EXCHANGE_MISMATCH =>
        Utilities.createInfoMessageBox(
          "Server denied request",
          "Server replied with " + responseMessage.message
        )
        Logger.error("Cannot continue with key exchange. Server responded: " + responseMessage.message, this)
        false
      case _ =>
        Utilities.createInfoMessageBox(
          "Server denied request",
          "Server responded unexpectedly: " + responseMessage.message + ", with symbol: " + responseMessage.symbol
        )
        Logger.error("Cannot continue with key exchange. Server responded unexpectedly: " + responseMessage.message + ", with symbol: " + responseMessage.symbol, this)
        false
    }
  }

  /*
   * exchangeRotationPolarisation() -> PHASE 2.5: This function checks with the server if it is accepting key exchanges!
   * @output: Boolean containing whether or not we can proceed!
   */
  def exchangeRotationPolarisation(): Boolean = {
    Logger.warn("Rotational Polarisation is enabled. This is an experimental feature", this)

    Logger.info("[INPUT REQUESTED] Requesting input from user for RP passphrase", this)
    val input = scala.swing.Dialog.showInput(null, "Enter your encryption passphrase for key (or 'none' for no key - unsafe!)", initial = "none")
    var key = "none"

    input match {
      case Some(s) => key = s
      case None =>
    }

    if (key.toLowerCase != "none") {
      partner.polarisation_key = Utilities.conformBinaryKey(Utilities.stringToBinaryString(key))
      partner.polarisation_key = BigInt(partner.polarisation_key, 2).toString(16)
    }

    Logger.info("[INPUT REQUESTED] Requesting input from user for RP degree", this)
    val RPInput = scala.swing.Dialog.showInput(null, "Enter your requested polarisation (in degrees from 0 to 360)", initial = "30")
    var degrees = 0

    RPInput match {
      case Some(s) => degrees = s.replaceAll("[^\\d.]", "").toInt % 360 // Convert entered number into integer (modulo 360).
      case None =>
    }

    partner.r_polarisation = degrees

    val degreesMessage = new ClassicalMessage("", "")

    val responseMessage = if (key != "none") {
      Logger.info("Sending the rotational polarisation degrees to server with encryption!", this)

      degreesMessage.symbol = ClassicalMessage.ENCRYPTED_DEGREES
      degreesMessage.message = Utilities.encryptWithAESCBC(partner.r_polarisation.toString, partner.polarisation_key)

      sendClassicalMessage(degreesMessage, encrypted = true, waitForResponse = true)
    }
    else {
      Logger.warn("You are sending the polarisation in plaintext! This is insecure and will not grant any extra security!", this)

      degreesMessage.symbol = ClassicalMessage.PLAINTEXT_DEGREES
      degreesMessage.message = partner.r_polarisation.toString

      sendClassicalMessage(degreesMessage, encrypted = false, waitForResponse = true)
    }


    if (responseMessage.symbol == ClassicalMessage.DEGREES_ACKNOWLEDGED) {
      Utilities.createInfoMessageBox(
        "Rotational polarisation agreed!",
        "Successfully agreed a rotational polarisation degree with server " + ip + ":" + port + "\nThe degree is: " + partner.r_polarisation
      )
      true
    }
    else if (responseMessage.symbol == ClassicalMessage.RP_DECLINED) {
      val res = scala.swing.Dialog.showConfirmation(null, "Server declined to use rotational polarisation, continue?", optionType = scala.swing.Dialog.Options.YesNo, title = "Server declined RP")

      if (res == scala.swing.Dialog.Result.Ok)
        Logger.warn("Client has chosen to resume with rotational polarisation DISABLED. Reverting to normal quantum channel", this)
        partner.r_polarisation = 0 // Disengage RP.
        true
    }
    else {
      Utilities.createInfoMessageBox(
        "Error!",
        "Server responded in an unexpected way. Response: " + responseMessage.message
      )
      Logger.error("Server responded in an unexpected way. Response: " + responseMessage.message, this)
      false
    }
  }

  /*
   * sendQuantumBits() -> PHASE 3: This function will proceed with sharing the quantum bits to the server!
   * @output: Boolean containing whether or not this completed successfully!
   */
  def sendQuantumBits(): Boolean = {
    // First we will acquire our basis bits
    partner.basis_bits = TRandom.requestRandomBits(partner.key_bits.length)

    breakable {
      // We now send each bit of the key, encoded by the basis.
      for (i <- 0 until partner.key_bits.length) {
        val quantumMessage = new QuantumMessage(partner.key_bits.charAt(i), partner.basis_bits.charAt(i), partner.r_polarisation)

        val responseMessage = sendQuantumMessage(quantumMessage, waitForResponse = true)

        if (responseMessage.symbol != ClassicalMessage.DATA_ACKNOWLEDGED)
          return false
      }
    }

    val completedMessage = new ClassicalMessage("", "")
    completedMessage.symbol = ClassicalMessage.KEY_EXCHANGE_COMPLETE
    completedMessage.message = partner.key_bits.length.toString

    val responseMessage = sendClassicalMessage(completedMessage, waitForResponse = true)
    if (responseMessage.symbol == ClassicalMessage.DATA_ACKNOWLEDGED) {
      Utilities.createInfoMessageBox(
        "Successful quantum transmission!",
        "Server received all of our quantum bits!"
      )
      true
    }
    else {
      Utilities.createInfoMessageBox(
        "Error!",
        "Server responded in an unexpected way. Response: " + responseMessage.message
      )
      Logger.error("Cannot continue with key exchange. Server responded with: " + responseMessage.message, this)
      false
    }
  }

  /*
   * shareBasisBits() -> PHASE 4: This function will proceed with sharing the basis bits publicly to the server!
   * @output: Boolean containing whether or not this completed successfully!
   */
  def shareBasisBits(): Boolean = {
    val basisMessage: ClassicalMessage = new ClassicalMessage("", "")

    basisMessage.symbol = ClassicalMessage.BASIS_EXCHANGE
    basisMessage.message = partner.basis_bits

    val responseMessage = sendClassicalMessage(basisMessage, waitForResponse = true)

    if (responseMessage.symbol == ClassicalMessage.BASIS_EXCHANGE) {
      partner.partner_basis_bits = responseMessage.message
      Utilities.createInfoMessageBox(
        "Successfully distributed basis bits!",
        "Server has received our basis bit string successfully"
      )
      true
    }
    else {
      Utilities.createInfoMessageBox(
        "Error!",
        "Server responded in an unexpected way. Response: " + responseMessage.message
      )
      Logger.error("Cannot continue with key exchange. Server responded with: " + responseMessage.message, this)
      false
    }
  }

  /*
   * computeFinalKey() -> PHASE 5: This function will compute the final key to be used with the server
   * @output: Final Key String that will be used for shared encryption (AES)
   */
  def computeFinalKey(): String = {
    // Construct a message to signify that we are now computing our key, and that the server should do the same
    val computeMessage = new ClassicalMessage("", "")
    computeMessage.symbol = ClassicalMessage.COMPUTE_KEY

    val responseMessage = sendClassicalMessage(computeMessage, waitForResponse = true)

    if (responseMessage.symbol == ClassicalMessage.DATA_ACKNOWLEDGED) {
      for (i <- 0 until partner.partner_basis_bits.length) {
        if (partner.partner_basis_bits.charAt(i) != partner.basis_bits.charAt(i))
          // Drop all the bits that the basis did not match
          partner.final_key += "_"
        else
          partner.final_key += partner.key_bits(i)
      }

      partner.final_key = partner.final_key.replace("_", "") // Dropped bits are now gone.
      partner.final_key = Utilities.conformBinaryKey(partner.final_key)
      partner.final_key = BigInt(partner.final_key, 2).toString(16) // Convert to a hexadecimal representation

      Logger.info("The final key is: " + partner.final_key, this)

      partner.encryption_ready = true
      partner.final_key
    }
    else {
      Utilities.createInfoMessageBox(
        "Error!",
        "Server responded in an unexpected way. Response: " + responseMessage.message
      )
      Logger.error("Cannot continue with key exchange. Server responded with: " + responseMessage.message, this)
      ""
    }
  }

  /*
   * agreeMorphingKeyMode() -> PHASE E1: This function will agree which morphing key mode to use if this option is ON!
   * @output: Morphing Function Chosen
   */
  def agreeMorphingKeyMode(): String = {
    val clientMorphingFunctions = "Q-MORPH-LIST->" + MorphingEngine.MorphingKeyManager.getHashingFunctions

    val response = sendEncryptedMessage(clientMorphingFunctions, partner.final_key, waitForResponse = true)

    if (response != null && response.message.startsWith("Q-MORPH-SEL->")) {
      // If we got a successful response...
      val function = response.message.split("->")(1) // Get the selected maximal-strength matching function
      partner.morphing_function = function

      if (randomMorphMode) {
        // Send that we wish to use random key morph intervals
        val countResponse = sendEncryptedMessage(ClassicalMessage.RANDOM_MODE, partner.final_key, waitForResponse = true)

        if (countResponse != null && countResponse.message == ClassicalMessage.RANDOM_MODE_ACCEPTED) {
          // If the server response with an accepting message of our random morph
          partner.morph_per_messages = -2
          partner.messages_since_morph = 0
          partner.random_morphing = true

          // Output to our log
          Logger.info("Mutating keys enabled!", this)
          Logger.info("Morphing interval is RANDOM", this)

          Utilities.createInfoMessageBox(
            "Mutating key encryption enabled!",
            "Mutating key encryption has been enabled. The morphing interval is RANDOM"
          )

          function
        }
        else if (countResponse == null) {
          Utilities.createInfoMessageBox(
            "Decryption error!",
            "Error during decryption, see log file for details."
          )
          Logger.error("Error during decryption, see above entries for details", this)
          "error"
        }
        else {
          // Unexpected response, print the message
          Utilities.createInfoMessageBox(
            "Error!",
            "Server responded in an unexpected way. Response: " + countResponse.message
          )
          Logger.error("Server replied unexpectedly with: " + countResponse.message, this)
          "none"
        }
      }
      else {
        // Send the number of messages we want to wait until we morph the key.
        val numberOfMessagesPerMorph = MorphingEngine.MorphingKeyManager.messagesPerMorph
        val countResponse = sendEncryptedMessage("Q-MORPH-COUNT->" + numberOfMessagesPerMorph, partner.final_key, waitForResponse = true)

        if (countResponse != null && countResponse.message == "Q-MORPH-ACK") {
          partner.morph_per_messages = numberOfMessagesPerMorph
          partner.messages_since_morph = 0

          Logger.info("Mutation keys enabled! Messages per mutation: " + numberOfMessagesPerMorph, this)

          function
        }
        else if (countResponse == null) {
          Logger.error("Error during decryption, see above entries for details", this)
          "error"
        }
        else {
          Logger.error("Server replied unexpectedly with: " + countResponse.message, this)
          "none"
        }
      }
    }
    else if (response != null && response.message.equals("Q-MORPH-UNMATCHABLE")) {
      Logger.warn("Server had no matching morphing function. Turning off mutating keys!", this)

      val res = scala.swing.Dialog.showConfirmation(null, "There are no matching morphing functions on this server, continue without mutating key encryption?", optionType = scala.swing.Dialog.Options.YesNo, title = "No matching functions")

      if (res == scala.swing.Dialog.Result.Ok)
        Logger.warn("Client has chosen to resume with mutating keys DISABLED. Reverting to static key encryption", this)
        partner.morphing_function = "none"
        partner.morph_per_messages = -1

      "none"
    }
    else if (response != null && response.symbol == ClassicalMessage.RESET) {
      Logger.error("The server responded with: " + response.message, this)
      "error"
    }
    else if (response == null) {
      Logger.error("Error during decryption, see above entries for details", this)
      "error"
    }
    else {
      Logger.error("Server replied unexpectedly with: " + response.message, this)
      "error"
    }
  }

  /*
   * sendReadySignal() -> PHASE 6: This function will agree which morphing key mode to use if this option is ON!
   * @output: Morphing Function Chosen
   */
  def sendReadySignal(): Boolean = {
    val readyMessage = ClassicalMessage.READY

    // Construct encrypted classical message
    val responseMessage = sendEncryptedMessage(readyMessage, key = partner.final_key, waitForResponse = true)

    // Process request
    if (responseMessage != null && responseMessage.message == ClassicalMessage.READY_ACKNOWLEDGED) {
      if (partner.random_morphing) { // Generate first interval for the key.
        partner.messages_since_morph = 0
        partner.morph_per_messages = scala.util.Random.nextInt(MorphingEngine.MorphingKeyManager.messagesPerMorph) + 1
      }
      true
    }
    else {
      Utilities.createInfoMessageBox(
        "Error!",
        "Server responded in an unexpected way. See log file for details."
      )
      Logger.error("Something went wrong, see above entries for details", this)
      false
    }
  }

  /*
   * sendClassicalMessage() -> Sends a classical message to the server
   * @input message: ClientMessage to be sent
   * @input waitForResponse: Whether we should wait for the server to respond.
   * @output: Response that was sent back by the server.
   */
  def sendClassicalMessage(message: ClassicalMessage, encrypted: Boolean = false, waitForResponse: Boolean = true): ClassicalMessage = {
    val clientSocket = new Socket(InetAddress.getByName(ip), port.toInt)
    lazy val in = new BufferedSource(clientSocket.getInputStream).getLines()
    val out = new PrintStream(clientSocket.getOutputStream)

    if (!encrypted)
      Logger.info("Sending: " + message.message, this)

    out.println(message.toString())
    out.flush()

    if (waitForResponse) {
      var response = ""
      response = in.next()

      val responseMessage = new ClassicalMessage(response) // De-serialise the response

      if (!encrypted)  // Don't print encrypted response
        Logger.info("Response: " + responseMessage.message, this)

      clientSocket.close()
      return responseMessage
    }

    clientSocket.close()
    null // Return statement
  }

  def sendEncryptedMessage(message: String, key: String, waitForResponse: Boolean = true): ClassicalMessage = {
    if (!partner.encryption_ready) {
      Logger.error("Server '" + partner.partner_ip + ":" + partner.partner_port + "' is not ready for encrypted communications.", this)
      return null
    }

    Logger.info("Sending encrypted: " + message, this)

    val encryptedMessage = new ClassicalMessage("", "")
    // Encrypt our plaintext with the secret key
    val cipherText = Utilities.encryptWithAESCBC(message + "<decrypt:ok>", key)

    // Construct our classical message
    encryptedMessage.symbol = ClassicalMessage.ENCRYPTED_MESSAGE
    encryptedMessage.message = cipherText

    // Get the response
    val responseMessage = sendClassicalMessage(encryptedMessage, encrypted = true, waitForResponse = waitForResponse)

    // Decrypt the cipher-text
    val responseCipherText = responseMessage.message
    try {
      if (message == ClassicalMessage.RANDOM_MORPH_NOW) {
        responseMessage.message = Utilities.decryptWithAESCBC(responseCipherText, Utilities.morphKey(key, partner.morphing_function))

      } else {
        responseMessage.message = Utilities.decryptWithAESCBC(responseCipherText, key)
      }
    }
    catch {
      case illex: IllegalArgumentException =>
        Logger.warn("There was an error decrypting message. This could mean the server has a mismatched key and has warned us in plaintext.", this)
        Logger.warn("Server responded with: " + responseMessage.message, this)
        return null
    }

    // Check that it was successful
    if (responseMessage.message == null) {
      Logger.error("Decryption failed. Cipher Text: " + responseCipherText + " Our sent message: " + message, this)
      return null
    }

    val successfulDecryption = responseMessage.message.contains("<decrypt:ok>")

    if (!successfulDecryption) {
      Logger.error("Decryption failed. Cipher Text: " + responseCipherText + " Our sent message: " + message, this)
      return null
    }

    responseMessage.message = responseMessage.message.replace("<decrypt:ok>", "")

    if (partner.secure_communications_ready) {
      partner.messages_since_morph += 1

      if (partner.messages_since_morph == partner.morph_per_messages) {
        partner.final_key = Utilities.morphKey(partner.final_key, partner.morphing_function)
        Logger.info("Successfully morphed the key using " + partner.morphing_function, this)
        partner.messages_since_morph = 0

        if (partner.random_morphing) {
          partner.messages_since_morph = -1000 // Prevents dual morphing during sending of encrypted morph signal.
          val response = sendEncryptedMessage(ClassicalMessage.RANDOM_MORPH_NOW, key, waitForResponse = true) // Sends morph signal using the old key.

          if (response.message == ClassicalMessage.RANDOM_MORPH_SUCCESS) {
            partner.messages_since_morph = 0
            partner.morph_per_messages = scala.util.Random.nextInt(MorphingEngine.MorphingKeyManager.messagesPerMorph) + 1
          }
        }
      }
    }

    responseMessage
  }

  /*
   * sendQuantumMessage() -> Sends a quantum message to the server
   * @input message: QuantumMessage to be sent
   * @input waitForResponse: Whether we should wait for the server to respond.
   * @output: Response that was sent back by the server.
   */
  def sendQuantumMessage(message: QuantumMessage, waitForResponse: Boolean = true): ClassicalMessage = {
    val clientSocket = new Socket(InetAddress.getByName(ip), port.toInt)
    lazy val in = new BufferedSource(clientSocket.getInputStream).getLines()
    val out = new PrintStream(clientSocket.getOutputStream)

    Logger.info("Sending quantum bit (cannot be displayed)", this)

    out.println(message.toString)
    out.flush()

    if (waitForResponse) {
      var response = ""
      response = in.next()
      val responseMessage = new ClassicalMessage(response)
      Logger.info("Response: " + responseMessage.message, this)

      clientSocket.close()
      return responseMessage
    }

    clientSocket.close()
    null
  }

  /*
   * sendSecretMessage() -> Sends an encrypted message string to the server using default keys
   * @input message: Message string that needs to be sent
   * @output Response as a string
   */
  def sendSecretMessage(message: String): String = {
    sendEncryptedMessage(message, key = partner.final_key, waitForResponse = true).message
  }
}
