import java.awt.{Color, Dimension, Font}
import java.net.Inet4Address

import MorphingEngine.MorphingKeyManager
import RandomEngine.{TrueRandomSource => TRandom}

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.{ButtonClicked, EditDone, WindowClosing}

/**
  * Created by annie on 18/03/2016.
  */
object BB84Simulator {
  val AES_KEYSIZE = 128 // Key Size for AES Encryption

  // Clients and Servers Databases in the form of ConcurrentHashMaps.
  val clients = new java.util.concurrent.ConcurrentHashMap[Inet4Address, Principal]
  val servers = new java.util.concurrent.ConcurrentHashMap[Inet4Address, Principal]

  object GUI extends SimpleSwingApplication {
    // Create server object.
    val server = new ExchangeServer(9999)
    server.startServer()

    override def top: Frame = new MainFrame {
      //// TITLE COMPONENT ////
      val titleLabel = new Label {
        text = "QuantumSim (Beta)"
        font = new Font("Arial", java.awt.Font.PLAIN, 24)
      }

      //// CENTRAL PANEL COMPONENTS ////
      val IP = new TextField { columns = 16; text = "127.0.0.1" }
      val Port = new TextField { columns = 5; text = "9999"; enabled = false }

      val hostNamePanel = new GridPanel(1, 2) {
        contents += new Label { text = "IP:  " }
        contents += IP
      }

      val portPanel = new GridPanel(1, 2) {
        contents += new Label { text = "Port:  " }
        contents += Port
      }

      val useMKConnect = new CheckBox { text = "Use Morphing Key"; selected = true }
      val useRPConnect = new CheckBox { text = "Use R. Polarisation" }

      val connectSettingsPanel = new GridPanel(1, 2) {
        contents += useMKConnect
        contents += useRPConnect
      }

      val connectButton = new Button {
        text = "Connect!"
        foreground = Color.green
        borderPainted = true
        enabled = true
        tooltip = "Click to initiate QKD using BB84"
      }

      val central = new GridPanel(4, 1) {
        contents += hostNamePanel
        contents += portPanel
        contents += connectSettingsPanel
        contents += connectButton
      }

      //// SETTINGS PANEL COMPONENTS ////
      val toggleButton = new ToggleButton { text = "Server"; selected = true }
      val toggleMorph = new ToggleButton { text = "Accept MK"; selected = true }
      val toggleRP = new ToggleButton { text = "Accept RP"; selected = true }

      val settingsLabel = new Label {
        text = "Server Settings"
        font = new Font("Arial", java.awt.Font.BOLD, 14)
      }

      val morphPerMessages = new TextField { columns = 3; text = "" + MorphingKeyManager.messagesPerMorph }
      val morphRandChkBox = new CheckBox { text = "Random?" }

      val settingsPanel = new GridPanel(3, 1) {
        contents += settingsLabel
        contents += new GridPanel(1, 3) {
          contents += toggleButton
          contents += toggleMorph
          contents += toggleRP
        }
        contents += new GridPanel(1, 3) {
          contents += new Label { text = "Msgs/morph: " }
          contents += morphPerMessages
          contents += morphRandChkBox
        }
      }

      //// ADD PANELS TO MAIN WINDOW PANEL ////
      contents = new BorderPanel {
        layout(titleLabel) = North
        // layout() = West
        layout(central) = Center
        // layout() = East
        layout(settingsPanel) = South
      }

      size = new Dimension(300, 275)

      // Which components will we want to listen to for events?
      listenTo(connectButton)
      listenTo(morphRandChkBox)
      listenTo(morphPerMessages)

      // Add reactions to these events
      reactions += {
        case ButtonClicked(component) if component == connectButton =>
          connectButton.enabled = false
          connectToClient(IP.text, Port.text.toInt, useMKConnect.selected, morphRandChkBox.selected, useRPConnect.selected)
          connectButton.enabled = true

        case ButtonClicked(component) if component == morphRandChkBox => // If MorphRand selected, disable per-message setting
          morphPerMessages.enabled = !morphRandChkBox.selected

        case EditDone(`morphPerMessages`) =>
          val newAmount = morphPerMessages.text.replaceAll("[^0-9]", "")
          morphPerMessages.text = newAmount
          MorphingKeyManager.messagesPerMorph = newAmount.toInt


        case WindowClosing(_) => // Gracefully close the server on WindowClosing
          server.stopServer()
      }
    }
  }

  def connectToClient(host: String = "127.0.0.1", port: Int = 9999, mk: Boolean = true, mr: Boolean, rp: Boolean = false): Unit = {
    // Create new Client Object
    val client = new ExchangeClient(host, port, mk, mr, rp)

    // Perform the Key Exchange
    client.executeKeyExchange()

      // Send some test messages -- TODO: For each client connected, create new GUI for sending messages.
    if (client.partner.secure_communications_ready) {
      client.sendSecretMessage("Hello!")
      client.sendSecretMessage("World!")
      client.sendSecretMessage("Testing!")
      client.sendSecretMessage("Stuff!")
      client.sendSecretMessage("Extra Stuff!")

    }
    else {
      Logging.Logger.error("BB84 Key Exchange has been aborted with " + client.partner.partner_ip, this)
    }
  }

  def main(args: Array[String]): Unit = {
    // Initialise the RandomManager and MorphingManager
    MorphingKeyManager.initialize()
    TRandom.initialize()

    if (TRandom.numberOfRandomBitsRemaining == 0) {
      Dialog.showMessage(
        null,
        "Could not get any true-random bits from Random.org :(",
        "QuantumSim has run into a problem",
        scala.swing.Dialog.Message.Error
      )
      return
    }

    GUI.main(args) // Initialise the GUI
  }
}
