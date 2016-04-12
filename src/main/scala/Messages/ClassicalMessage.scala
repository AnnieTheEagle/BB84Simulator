package Messages

/**
  * Created by annie on 21/03/2016.
  */
object ClassicalMessage {
  // Key Exchange
  val KEY_EXCHANGE_INIT = "Q-INIT"
  val KEY_EXCHANGE_ACCEPT = "Q-EX-ACC"
  val KEY_EXCHANGE_DENIED = "Q-EX-DEN"
  val KEY_EXCHANGE_MISMATCH = "Q-EX-KSM"
  val KEY_EXCHANGE_COMPLETE = "Q-EX-COMPLETE"

  // Basis Exchange
  val BASIS_EXCHANGE = "Q-BASIS"
  val COMPUTE_KEY = "Q-KEY-COMPUTE"

  // Encrypted Communications
  val ENCRYPTED_MESSAGE = "ENCRYPTED"

  // Morphing Key
  val RANDOM_MODE = "Q-MORPH-RAND"
  val RANDOM_MODE_ACCEPTED = "Q-MORPH-RAND-ACK"
  val RANDOM_MORPH_NOW = "Q-MUTATE"
  val RANDOM_MORPH_SUCCESS = "Q-MUTATE-OK"

  // Rotational Polarisation
  val ENCRYPTED_DEGREES = "Q-RDEG-ENC"
  val PLAINTEXT_DEGREES = "Q-RDEG-PLN"
  val DEGREES_ACKNOWLEDGED = "Q-RDEG-ACK"

  // Ready Signal
  val READY = "Q-READY"
  val READY_ACKNOWLEDGED = "Q-READY-ACK"

  // General Purpose
  val DATA_ACKNOWLEDGED = "Q-ACK"
  val RESET = "Q-RST"
  val DECRYPT_ERROR = "Q-DECRYPT-ERROR"
}

class ClassicalMessage (var symbol: String, var message: String) {
  def this(serialized: String) {
    this("", "")
    val parts = serialized.split("§§")

    this.symbol = parts(1).stripPrefix("{")
    this.message = parts(2).stripSuffix("}")
  }

  override def toString: String = {
    var string = ""
    string += "{CLASSICAL§§"
    string += symbol
    string += "§§"
    string += message
    string += "}"

    string
  }
}
