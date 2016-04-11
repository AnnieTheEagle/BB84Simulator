import javax.crypto.{BadPaddingException, Cipher}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import scalaj.http.Base64

/**
  * Created by annie on 18/03/2016.
  */
object Utilities {
  /*
   * encodePassphrase() -> Encodes a passphrase using the orthogonal basis (diagonal 'd' and rectilinear 'r').
   * @input key: Binary-string representation of the key.
   * @input basis: Binary-string representation of the basis.
   * @output: Encoded passphrase string
   */
  def encodePassphrase(key: String, basis: String): String = { // Method to encode the key using basis.
  var encoded = ""
    for (i <- 0 until key.length) {
      var base = { if (basis.charAt(i) == '1') "d" else "r" } // If basis bit is 1, use diagonal basis, otherwise rectilinear.

      if (key.charAt(i) == '1') base = base.toUpperCase // If key bit is 1, use uppercase, otherwise lowercase.
      encoded = encoded + base
    }

    encoded // Return encoded passphrase
  }

  /*
   * encryptWithAESCBC() -> Encrypts an input string with a 128-bit key using AES with CBC mode, not default ECB crap.
   * @input pText: Plaintext message that is to be encrypted.
   * @input key: Binary string representation of key.
   * @output: Ciphertext which is equal to encryption of pText with key
   */
  def encryptWithAESCBC(pText: String, key: String): String = {

    val keyBytes = hexStringToByteArray(key) // 128-bit key represented as an Array[Byte] of size 16.
    val ivBytes = "@bb84simulator!#".getBytes("UTF-8") // 16-byte IV

    val aesKey = new SecretKeySpec(keyBytes, "AES") // Create new AES Key

    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING") // Create new AES Instance with CBC mode and PKCS5 Padding.
    cipher.init(Cipher.ENCRYPT_MODE, aesKey, new IvParameterSpec(ivBytes)) // Initialise with key and IV in Encrypt Mode.

    val result = cipher.doFinal(pText.getBytes()) // Encrypt the data

    val cipherText = new String(Base64.encode(result)) // Encode it into base64 for transport.

    cipherText // Return cipher text
  }

  /*
   * decryptWithAESCBC() -> Decrypt an input string with a 128-bit key using AES.
   * @input cText: Ciphertext message that is to be decrypted.
   * @input key: Binary string representation of key.
   * @output: Plaintext which is equal to decryption of cText with key
   */
  def decryptWithAESCBC(cText: String, key: String): String = {
    val keyBytes = hexStringToByteArray(key) // 128-bit key
    val ivBytes = "@bb84simulator!#".getBytes("UTF-8") // 16-byte IV

    val aesKey = new SecretKeySpec(keyBytes, "AES") // Create new AES Key

    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING") // Create new AES Instance with CBC mode and PKCS5 Padding.
    cipher.init(Cipher.DECRYPT_MODE, aesKey, new IvParameterSpec(ivBytes)) // Initialise with key and IV in Decrypt Mode.

    try {
      val result = cipher.doFinal(Base64.decode(cText)) // Decode the text then decrypt.
      new String(result) // Return plaintext
    }
    catch {
      case bpEx: BadPaddingException =>
        Logging.Logger.error("Cannot decrypt this string. Potential cause: Wrong Key!", this)
        null
    }


  }

  /* hexStringToByteArray() -> Converts the input hex string into a array of bytes.
   * @input inputStr: Hexadecimal string to be converted into byte array.
   * @output: Array of bytes equal to the hexadecimal string.
   */
  def hexStringToByteArray(inputStr : String) = {
    val stringLength = inputStr.length()
    val bytes = new Array[Byte](stringLength / 2)
    var i = 0

    while (i < stringLength - 1) {
      val byteInt = (Character.digit(inputStr.charAt(i), 16) << 4) + Character.digit(inputStr.charAt(i + 1), 16)

      bytes(i / 2) = byteInt.asInstanceOf[Byte]

      i += 2 // Increase character index by two (two hexadecimal characters per byte).
    }

    bytes // Return array.
  }

  /*
   * morphKey() -> Transforms the key string into a new key using morphing functions.
   * @input keyString: Hexadecimal string representation of key to be transformed.
   * @input morphRule: Binary string representation of key to be conformed.
   * @output: New binary string of key that is 128-bits long
   */
  def morphKey(keyString: String, morphRule: String): String = {
    morphRule match {
      case "SHA512" =>
        MorphingEngine.MorphingKeyManager.getSHA512(keyString).substring(0, 32) // Get SHA-512 Hash

      case "SHA256" =>
        MorphingEngine.MorphingKeyManager.getSHA256(keyString).substring(0, 32) // Get SHA-256 Hash

      case "MD5" =>
        MorphingEngine.MorphingKeyManager.getMD5(keyString).substring(0, 32) // Get MD5 Hash

      case _ => // Default statement (no morphing rule found).
        Logging.Logger.error("MorphingKeyManager", "An unknown morphing rule has been selected. Cannot morph key.")
        keyString
    }
  }

  /*
   * conformBinaryKey() -> Modifies key string to ensure it is 128-bits long.
   * @input keyStr: Binary string representation of key to be conformed.
   * @output: New binary string of key that is 128-bits long
   */
  def conformBinaryKey(keyStr: String): String = {
    keyStr.length.compare(BB84Simulator.AES_KEYSIZE) match {
      case 0 => keyStr // Key is of perfect length
      case 1 => keyStr.substring(0, BB84Simulator.AES_KEYSIZE) // Take first n bits of key
      case -1 => // Key is not long enough
        val newKey = new StringBuilder((2 * BB84Simulator.AES_KEYSIZE) - 1)

        while (newKey.length < BB84Simulator.AES_KEYSIZE)
          newKey.append(keyStr)

        newKey.setLength(BB84Simulator.AES_KEYSIZE)
        newKey.toString() // Return new key
    }
  }

  /*
 * stringToBinaryString() -> Converts a string into a binary string representation.
 * @input text: Input string of characters to be converted.
 * @output: String containing the binary representation of the string
 */
  def stringToBinaryString(text: String): String = {
    var str = ""
    for (byte <- text.getBytes) { // Convert input string into bytes, then foreach byte:
      // Convert integer (unsigned byte) into a string replacing ' ' with 0s.
      str = str.concat(String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0'))
    }

    str // Return new string
  }
}
