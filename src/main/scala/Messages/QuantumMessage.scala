package Messages

import RandomEngine.TrueRandomSource


/**
  * QuantumMessage.scala
  * This file describes a pseudo-quantum message and how they are serialised for communcation.
  */
object QuantumMessage {
  val PSEUDO_FACTOR = 1000000000
  val PSEUDO_RANDOM = new scala.util.Random()
}

class QuantumMessage (bit: Char, basis: Char, polarisation: Int) {
  // MARK: Fields
  private var superposed: Boolean = true
  private var qubit: Int = -999 // -999 = Undefined
  private var observedBit: Char = 'X'
  private var originalBasis: Char = basis
  private var originalPolarisation: Int = polarisation

  def this(serialized: String) {
    this('X', 'X', 0)
    val parts = serialized.split("§§")

    this.superposed = if (parts(1).stripPrefix("{") == "1") true else false
    this.qubit = parts(2).toInt
    this.observedBit = parts(3).charAt(0)
    this.originalBasis = parts(4).charAt(0)
    this.originalPolarisation = parts(5).stripSuffix("}").toInt
  }

  // Initialisation
  if (bit == '1' && basis == Basis.DIAGONAL) {
    qubit = 135
  }
  else if (bit == '0' && basis == Basis.DIAGONAL) {
    qubit = 45
  }
  else if (bit == '1' && basis == Basis.RECTILINEAR) {
    qubit = 90
  }
  else if (bit == '0' && basis == Basis.RECTILINEAR) {
    qubit = 0
  }
  else {
    superposed = false
  }

  if (superposed) {
    qubit += originalPolarisation
  }

  def measureQubitSystem(measuringBasis: Char, polarisation: Int): Char = {
    if (observedBit != -999 && !superposed) { // If this system has already been observed
      observedBit
    }

    else {
      superposed = false

      observedBit = if (polarisation == originalPolarisation) {
        if (originalBasis == measuringBasis) {
          val originalState = qubit - originalPolarisation

          (originalState / 90).toString.charAt(0) // (Only the original BB84 states 90 and 135 will return a 1 bit, so this is correct)
        }

        else {
          TrueRandomSource.requestRandomBits(1).charAt(0)
        }
      }
      else {
        var diff = polarisation - originalPolarisation
        if (originalBasis != measuringBasis) {
          diff += 45
        }

        /* Read the bit with a probability. Explanation:
         * Since the difference of our basis is equal to the offset of the recipient polarizer and the sender.
         * If the difference is very small, we have a high chance of reading the correct bit, (increased by 45 degrees
         * for wrong basis). This can be made into a sin wave-like chance to read the wrong or right bit
         * At 1 degree offset, we have a very high chance to read the right bit (if right basis).
         * At 89 degrees offset, we have a very high chance to read the wrong bit (even if right basis). Therefore
         * we can use the cosine wave (1 at 0, 0 at 90) to calculate the chance to get the right value.
         */

        // Generated check value between 1 and 1,000,000,000
        val gen = QuantumMessage.PSEUDO_RANDOM.nextInt(QuantumMessage.PSEUDO_FACTOR)

        // Check value is equal to the squared cosine wave
        val check = (Math.pow(Math.cos(Math.toRadians(diff)), 2) * QuantumMessage.PSEUDO_FACTOR).toInt

        val originalState = qubit - originalPolarisation
        val originalBit = (originalState / 90).toString.charAt(0)

        if (gen < check) {
          originalBit
        }
        else {
          if (originalBit == '1') '0' else '1'
        }
      }

      observedBit
    }
  }



  override def toString: String = {
    var string = ""
    string += "{QUANTUM§§"
    string += (if (superposed) "1" else "0")
    string += "§§"
    string += qubit
    string += "§§"
    string += observedBit
    string += "§§"
    string += originalBasis
    string += "§§"
    string += originalPolarisation
    string += "}"

    string
  }
}
