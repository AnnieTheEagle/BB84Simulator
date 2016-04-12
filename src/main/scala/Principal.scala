/**
  * Principal.scala
  *
  * Class to hold all the information on a principal (this can be used by both the server or the client).
  */
class Principal(connectingTo: String) {
  val partner_ip = connectingTo.split(':')(0)
  val partner_port = connectingTo.split(':')(1)

  var key_string: String = "" // Key Passphrase (will always be empty for server)
  var key_bits: String = "" // Starts full for client, used by server to record bits as recorded

  var basis_bits: String = "" // Used by client to record basis bits sent, used by server to record measuring basis
  var partner_basis_bits: String = "" // Used to store the other principal's basis

  var morphing_function: String = "none" // Used to denote which morphing function if any
  var random_morphing: Boolean = false // Random Morphing Mode?
  var morph_per_messages: Int = -1 // Number of messages per morph
  var messages_since_morph: Int = -1 // Messages since last morph

  var polarisation_key: String = "3.14159" // Key used to encrypt polarisation degrees (default of '3.14159' for simulation purposes ONLY!)
  var r_polarisation: Int = 0 // Degrees used to polarise channel - If this is 0, then RP is off.

  var encryption_ready: Boolean = false // Ready to use encrypted communications!
  var secure_communications_ready: Boolean = false // All key exchange protocols and sub-protocols (MK, RP) completed?
  var final_key: String = "" // The final encryption key to use
}
