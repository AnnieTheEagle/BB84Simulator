import java.io.PrintStream
import java.net.{Inet4Address, InetSocketAddress, ServerSocket, SocketException}

import Logging.Logger
import Messages.{ClassicalMessage, QuantumMessage}
import MorphingEngine.MorphingKeyManager
import RandomEngine.{TrueRandomSource => TRandom}

import scala.io.BufferedSource

/**
  * ExchangeServer.scala
  * This runs the server daemon of the application used for accepting key exchange requests
  */
class ExchangeServer (port: Integer) {
  val server = new ServerSocket(port) // Create the socket server

  var acceptRequests = true // Declare whether we want to accept incoming requests
  var acceptRP = true // Declare whether we want to accept incoming RP requests

  val serverThread = new Thread(new Runnable {

    /*
     * run() -> Function to execute upon starting up the thread
     */
    def run() {
      Logger.info("Server is now running on port " + port, this)

      try {
        while (true) { // Loop forever, until we receive a socketException (server closing)
        val s = server.accept() // Wait for a message
        val in = new BufferedSource(s.getInputStream).getLines() // Get lines of message.
        val out = new PrintStream(s.getOutputStream) // Response stream

          val clientSocket = s.getRemoteSocketAddress.asInstanceOf[InetSocketAddress].getAddress // Get the client

          val response = processMessage(in.next(), clientSocket.asInstanceOf[Inet4Address]) // Generate response

          out.println(response.toString()) // Send the response back to the client

          out.flush()
          s.close()
        }
      }
      catch { // Handle socket closed exception
        case sockEx: SocketException => handleIOException(sockEx)
      }
      finally { // Always ensure we close this socket.
        server.close()
      }
    }

    /*
     * processMessage() -> Function for processing a message that has come into the server
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def processMessage(msg: String, address: Inet4Address): ClassicalMessage = {
      val quantumMessage = if (msg.charAt(1) == 'Q') true else false

      var response: ClassicalMessage = new ClassicalMessage("", "")
      response = if (!quantumMessage) {
        // De-serialise message into a classical message object (simulation)
        handleClassicalMessage(address, new ClassicalMessage(msg))
      }
      else {
        // De-serialise 'quantum system' into a quantum message object (simulation)
        handleQuantumMessage(address, new QuantumMessage(msg))
      }

      response // Return statement
    }

    /*
     * handleClassicalMessage() -> Handler for all non-quantum messages received by the server
     * @input sender: Source of the message
     * @input msg: Classical message object containing the data of the message.
     */
    def handleClassicalMessage(sender: Inet4Address, msg: ClassicalMessage): ClassicalMessage = {
      var response: ClassicalMessage = new ClassicalMessage("", "")
      response = msg match {
        case m if m.symbol == ClassicalMessage.KEY_EXCHANGE_INIT => handleInitialiseKeyExchange(msg, sender)

        case m if m.symbol == ClassicalMessage.KEY_EXCHANGE_COMPLETE => handleCompletionOfKeyBits(msg, sender)

        case m if m.symbol == ClassicalMessage.BASIS_EXCHANGE => handleBasisBitFromClient(msg, sender)

        case m if m.symbol == ClassicalMessage.COMPUTE_KEY => handleComputeKeyMessage(msg, sender)

        case m if m.symbol == ClassicalMessage.ENCRYPTED_MESSAGE => handleEncryptedMessage(msg, sender)

        case m if m.symbol == ClassicalMessage.ENCRYPTED_DEGREES => handleRotationalPolarisation(msg, sender, encrypted = true)

        case m if m.symbol == ClassicalMessage.PLAINTEXT_DEGREES => handleRotationalPolarisation(msg, sender, encrypted = false)

      }

      response
    }

    def handleQuantumMessage(sender: Inet4Address, msg: QuantumMessage): ClassicalMessage = {
      var response: ClassicalMessage = new ClassicalMessage("", "")

      response = handleKeyBit(msg, sender)

      response
    }

    /*
     * handleInitialiseKeyExchange() -> Function for handling the initiation of a key exchange
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleInitialiseKeyExchange(msg: ClassicalMessage, address: Inet4Address): ClassicalMessage = {
      val response: ClassicalMessage = new ClassicalMessage("", "")
      if (acceptRequests) {
        response.symbol = ClassicalMessage.KEY_EXCHANGE_ACCEPT
        response.message = "Accepted"
      }
      else {
        response.symbol = ClassicalMessage.KEY_EXCHANGE_DENIED
        response.message = "Denied"
      }
      try {
        val clientKeySize = msg.message.split(":")(1) // Get the key size from the initial message
        if (clientKeySize.toInt == BB84Simulator.AES_KEYSIZE)
          BB84Simulator.clients.put(address, new Principal(address.getHostAddress + ":" + port))
        else {
          response.symbol = ClassicalMessage.KEY_EXCHANGE_MISMATCH
          response.message = "Key-size mismatch. My key-size is '" + BB84Simulator.AES_KEYSIZE + "-bit'. Your key-size is '" + clientKeySize.toInt + "-bit'."
        }
      }
      catch {
        case nfeEx: NumberFormatException =>
          Logger.error("NumberFormatException in handleInitialiseKeyExchange()", this)
          response.symbol = ClassicalMessage.RESET
          response.message = "Recipient encountered a server-side error."
      }

      response
    }

    /*
     * handleKeyBit() -> Function for handling an incoming key bit
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleKeyBit(msg: QuantumMessage, address: Inet4Address): ClassicalMessage = {
      val response: ClassicalMessage = new ClassicalMessage("", "")

      // Check if there is an ongoing key-exchange with this client address.
      if (BB84Simulator.clients.containsKey(address)) {
        // var polarisation = Double.NegativeInfinity // -∞ = Undefined polarisation
        try {
          val client = BB84Simulator.clients.get(address)

          val basisBit = TRandom.requestRandomBits(1).charAt(0)
          val bitRead = msg.measureQubitSystem(basisBit, client.r_polarisation)

          // Add this data to our client list.
          client.key_bits += bitRead
          client.basis_bits += basisBit

          response.symbol = ClassicalMessage.DATA_ACKNOWLEDGED
          response.message = "Key bit received!"
        }
        catch {
          case nfeEx: NumberFormatException =>
            Logger.error("NumberFormatException in handleKeyBit()", this)
            response.symbol = ClassicalMessage.RESET
            response.message = "Recipient encountered a server-side error."
        }
      }
      else {
        // No matching active key-exchange.
        response.symbol = ClassicalMessage.RESET
        response.message = "Please initialise a key exchange properly."
      }

      response
    }

    /*
     * handleCompletionOfKeyBits() -> Function to ensure that all key bits are received
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleCompletionOfKeyBits(msg: ClassicalMessage, address: Inet4Address): ClassicalMessage = {
      val response: ClassicalMessage = new ClassicalMessage("", "")
      val client = BB84Simulator.clients.get(address)

      if (client == null) {
        response.symbol = ClassicalMessage.RESET
        response.message = "You are unknown to this server."
      }

      else if (msg.message.toInt == client.key_bits.length) {
        response.symbol = ClassicalMessage.DATA_ACKNOWLEDGED
        response.message = "All bits have been received successfully!"
      }

      else {
        response.symbol = ClassicalMessage.RESET
        response.message = "There is a mismatch in the number of bits received. You = '" + msg.message + "', Server = '" + client.key_bits + "'"
      }

      response
    }

    /*
     * handleBasisBitFromClient() -> Function for handling an incoming basis bit from client
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleBasisBitFromClient(msg: ClassicalMessage, address: Inet4Address): ClassicalMessage = {
      val response: ClassicalMessage = new ClassicalMessage("", "")

      // Check if there is an ongoing key-exchange with this client address.
      if (BB84Simulator.clients.containsKey(address)) {
        val basisString = msg.message
        val client = BB84Simulator.clients.get(address)
        client.partner_basis_bits = basisString

        // Send back the server's measured basis bit
        response.symbol = ClassicalMessage.BASIS_EXCHANGE
        response.message = client.basis_bits
      }
      else {
        // No matching active key-exchange.
        response.symbol = ClassicalMessage.RESET
        response.message = "You are unknown to this server."
      }

      response
    }

    /*
     * handleComputeKeyMessage() -> Function for handling an the client sending a signal to compute our final key
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleComputeKeyMessage(msg: ClassicalMessage, address: Inet4Address): ClassicalMessage = {
      val message: ClassicalMessage = new ClassicalMessage("", "")
      val client = BB84Simulator.clients.get(address)

      if (client == null) {
        // If this client is not in our register...
        message.symbol = ClassicalMessage.RESET
        message.message = "You are unknown to this server."
        return message
      }

      for (i <- 0 until client.partner_basis_bits.length) {
        if (client.partner_basis_bits.charAt(i) != client.basis_bits.charAt(i))
          client.final_key += "_"
        else
          client.final_key += client.key_bits(i)
      }

      client.final_key = client.final_key.replace("_", "")
      client.final_key = Utilities.conformBinaryKey(client.final_key)
      client.final_key = BigInt(client.final_key, 2).toString(16) // Convert to a hexadecimal representation

      Logger.info("The final key is: " + client.final_key, this)

      message.symbol = ClassicalMessage.DATA_ACKNOWLEDGED
      message.message = "Key calculated!"

      message
    }

    /*
     * handleEncryptedMessage() -> Function for handling an incoming encrypted message
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @output: Response to be sent back to the client
     */
    def handleEncryptedMessage(msg: ClassicalMessage, address: Inet4Address): ClassicalMessage = {
      val message: ClassicalMessage = new ClassicalMessage("", "")
      var response = ""
      val client = BB84Simulator.clients.get(address)

      if (client == null) {
        // If this client is not in our register... send a RESET packet.
        message.symbol = ClassicalMessage.RESET
        message.message = "You are unknown to this server."
        return message
      }

      val encryptionKey = client.final_key
      val encryptedMsg = msg.message

      val pT = Utilities.decryptWithAESCBC(encryptedMsg, encryptionKey)

      if (pT == null) {
        message.symbol = ClassicalMessage.DECRYPT_ERROR
        message.message = "Cannot decrypt your message, key error!"
        return message
      }

      val plainText = pT.replace("<decrypt:ok>", "") // Remove the message showing decryption was okay!

      plainText match {
        case p if p.startsWith("Q-MORPH-LIST") =>
          val f = getBestMorphingFunction(plainText.split("->")(1))
          if (f != "NONE") {
            client.morphing_function = f
            response = "Q-MORPH-SEL->" + f
          }
          else
            response = "Q-MORPH-UNMATCHABLE"

        case q if q.startsWith("Q-MORPH-COUNT") =>
          val f = plainText.split("->")(1).toInt
          client.morph_per_messages = f
          client.messages_since_morph = -1 // We start at minus 1 so that the 'Q-READY' sets it to 0
          response = "Q-MORPH-ACK"

        case q if q == ClassicalMessage.RANDOM_MODE =>
          client.morph_per_messages = -2
          client.messages_since_morph = -1 // We start at minus 1 so that the 'Q-READY' sets it to 0
          client.random_morphing = true
          response = ClassicalMessage.RANDOM_MODE_ACCEPTED

        case q if q == ClassicalMessage.READY =>
          client.secure_communications_ready = true
          response = ClassicalMessage.READY_ACKNOWLEDGED

        case q if q == ClassicalMessage.RANDOM_MORPH_NOW =>
          client.final_key = Utilities.morphKey(client.final_key, client.morphing_function)
          Logger.info("Key has been successfully mutated using " + client.morphing_function, this)
          response = ClassicalMessage.RANDOM_MORPH_SUCCESS

        case _ =>
          response = plainText.toUpperCase() // Default
      }

      Logger.info("Sending response: " + response, this)
      response = Utilities.encryptWithAESCBC(response + "<decrypt:ok>", BB84Simulator.clients.get(address).final_key)

      if (client.morphing_function != "none" && client.secure_communications_ready && pT.contains("<decrypt:ok>")) {
        if (!client.random_morphing)
          client.messages_since_morph += 1

        if (client.morph_per_messages == client.messages_since_morph) {
          client.final_key = Utilities.morphKey(client.final_key, client.morphing_function)
          Logger.info("Key has been successfully mutated using " + client.morphing_function, this)
          client.messages_since_morph = 0
        }
      }

      message.symbol = "ENCRYPTED"
      message.message = response

      message
    }

    /*
     * handleRotationalPolarisation() -> Function for handling the special case, rotational polarisation messages
     * @input msg: Incoming message that was sent by the client
     * @input address: Source of the message
     * @input encrypted: Whether or not the data was sent encrypted
     * @output: Response to be sent back to the client
     */
    def handleRotationalPolarisation(msg: ClassicalMessage, address: Inet4Address, encrypted: Boolean): ClassicalMessage = {
      val message: ClassicalMessage = new ClassicalMessage("", "")
      val client = BB84Simulator.clients.get(address)

      if (client == null) {
        // If this client is not in our register...
        message.symbol = ClassicalMessage.RESET
        message.message = "You are unknown to this server."
        return message
      }

      if (!acceptRP) {
        // If the server is currently declining to use RP polarised channels (for example, doesn't have the capability to do so)...
        message.symbol = ClassicalMessage.RP_DECLINED
        message.message = "This server is not accepting the use of rotationally polarised channels."
        return message
      }

      if (encrypted) {
        // Encryption key = "3.14159" - Simulation ONLY
        var encryptionKey = Utilities.conformBinaryKey(Utilities.stringToBinaryString(client.polarisation_key))
        encryptionKey = BigInt(encryptionKey, 2).toString(16)

        msg.message = Utilities.decryptWithAESCBC(msg.message, encryptionKey)

        if (msg.message == null) {
          message.symbol = ClassicalMessage.DECRYPT_ERROR
          message.message = "Cannot decrypt your message."
        }

        else {
          client.r_polarisation = msg.message.toInt

          message.symbol = ClassicalMessage.DEGREES_ACKNOWLEDGED
          message.message = "Rotational Polarisation is ready!"
        }
      }

      else {
        client.r_polarisation = msg.message.toInt

        message.symbol = ClassicalMessage.DEGREES_ACKNOWLEDGED
        message.message = "Rotational Polarisation is ready!"
      }

      message
    }

    /*
     * getBestMorphingFunction() -> Function for selecting the best mutating function for morphing keys.
     * @input list: List of morphing functions from client
     * @output: Best morphing function by security chosen to be used by both parties
     */
    def getBestMorphingFunction(list: String): String = {
      val clientList = list.split(',')
      val serverList = MorphingKeyManager.getHashingFunctions.split(',')

      for (h <- serverList) {
        // This simulator sorts hashing functions in descending order of strength, first match will be the strongest!
        if (clientList.contains(h)) return h
      }

      // No match
      "NONE"
    }

    /*
     * calculateFinalKey() -> Function to compare the server's basis bits with the used ones.
     *                        Any mismatched bits will then be discarded and ignored.
     * @input principal: The client principal object for which to calculate the key.
     */
    def calculateFinalKey(principal: Principal): Unit = {
      for (i <- 0 until principal.partner_basis_bits.length) {
        if (principal.basis_bits.charAt(i) != principal.partner_basis_bits.charAt(i))
          // If there's a mismatch, we can throw that bit away
          principal.final_key += "_"
        else
          principal.final_key += principal.key_bits(i)
      }

      principal.final_key = principal.final_key.replace("_", "") // Remove all junk bits
      principal.final_key = Utilities.conformBinaryKey(principal.final_key)
      principal.final_key = BigInt(principal.final_key, 2).toString(16)

      Logger.info("Final Key: " + principal.final_key, this)
    }

    /*
     * handleIOException() -> Function to handle any server IO exceptions
     * @input e: Exception object which was thrown
     */
    def handleIOException(e: SocketException): Unit = {
      // This is expected when closing the connection while waiting for a message (via server.accept())
      if (e.getMessage.toLowerCase == "socket closed")
        Logger.info("Server was stopped successfully.", this)
      else
        Logger.error("Something went wrong: " + e.getMessage, this)
    }
  })

  /*
   * startServer() -> Function to start the QKD Server!
   */
  def startServer() = {
    serverThread.start()
    Logger.info("Starting key-distribution server...", this)
  }

  /*
   * stopServer() -> Function to close the QKD Server!
   */
  def stopServer() = {
    server.close()
    Logger.info("Stopping key-distribution server...", this)
  }

}
