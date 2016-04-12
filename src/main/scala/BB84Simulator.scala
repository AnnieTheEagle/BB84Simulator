import java.awt.{Color, Dimension, Font}
import java.net.Inet4Address

import MorphingEngine.MorphingKeyManager
import RandomEngine.{TrueRandomSource => TRandom}

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.{ButtonClicked, EditDone, WindowClosing}
import scala.util.matching.Regex

/**
  * Created by annie on 18/03/2016.
  */
object BB84Simulator {
  val AES_KEYSIZE = 128 // Key Size for AES Encryption

  // Clients and Servers Databases in the form of ConcurrentHashMaps.
  val clients = new java.util.concurrent.ConcurrentHashMap[Inet4Address, Principal]
  val servers = new java.util.concurrent.ConcurrentHashMap[Inet4Address, Principal]

  // Regex Patterns
  val IP_REGEX = new Regex("^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")
  val NUMERIC_REGEX = new Regex("[^0-9]")

  object GUI extends SimpleSwingApplication {
    // Create server object.
    val server = new ExchangeServer(9999)
    server.startServer()

    override def top: Frame = new MainFrame {
      //// TITLE COMPONENT ////
      title = "BB84 Simulator by Ferdinand Keller"
      val titleLabel = new Label {
        text = "BB84Simulator (Beta)"
        font = new Font("Arial", java.awt.Font.PLAIN, 24)
      }

      //// CENTRAL PANEL COMPONENTS ////
      val IP = new TextField { columns = 16; text = "127.0.0.1" }
      val Port = new TextField { columns = 5; text = "9999" }

      val hostNamePanel = new GridPanel(1, 2) {
        contents += new Label { text = "Target IP: " }
        contents += IP
      }

      val portPanel = new GridPanel(1, 2) {
        contents += new Label { text = "Target Port: " }
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
      val toggleButton = new ToggleButton { text = "Accept Requests"; selected = true }
      val toggleRP = new ToggleButton { text = "Accept RP"; selected = true }

      val serverSettingsLabel = new Label {
        text = "Server Settings"
        font = new Font("Arial", java.awt.Font.BOLD, 14)
      }
      val clientSettingsLabel = new Label {
        text = "Client Settings"
        font = new Font("Arial", java.awt.Font.BOLD, 14)
      }

      val morphPerMessages = new TextField { columns = 3; text = "" + MorphingKeyManager.messagesPerMorph }
      val morphRandChkBox = new CheckBox { text = "Random?" }

      val messageLabel = new Label { text = "Msgs/morph: " }

      val settingsPanel = new GridPanel(4, 1) {
        contents += serverSettingsLabel
        contents += new GridPanel(1, 2) {
          contents += toggleButton
          contents += toggleRP
        }
        contents += clientSettingsLabel
        contents += new GridPanel(1, 3) {
          contents += messageLabel
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

      // Setup listener for our components that have actions/events.
      listenTo(connectButton)
      listenTo(toggleRP)
      listenTo(toggleButton)
      listenTo(morphRandChkBox)
      listenTo(morphPerMessages)

      // Add reactions to these events
      reactions += {
        case ButtonClicked(component) if component == connectButton =>
          // If the connect button has been pushed, connect to target client.
          connectButton.enabled = false
          connectToClient(IP.text, Port.text.toInt, useMKConnect.selected, morphRandChkBox.selected, useRPConnect.selected)
          connectButton.enabled = true

        case ButtonClicked(component) if component == toggleRP =>
          // Toggle whether or not the server will accept or decline RP
          server.acceptRP = toggleRP.selected
          toggleRP.text = if (toggleRP.selected) "Accept RP" else "Decline RP"

        case ButtonClicked(component) if component == toggleButton =>
          // Toggle whether or not the server will accept or decline key exchange requests
          server.acceptRequests = toggleButton.selected
          toggleButton.text = if (toggleButton.selected) "Accept Requests" else "Decline Requests"

        case ButtonClicked(component) if component == morphRandChkBox =>
          // If random mutation mode is selected, change to maximum number of messages (upper bound on the randomisation of the next slice size)
          messageLabel.text = { if (morphRandChkBox.selected) "Max/morph: " else "Msgs/morph: " }

        case EditDone(`morphPerMessages`) =>
          // Once we finish editing morphPerMessages, replace all non numeric characters with nothing.
          val newAmount = NUMERIC_REGEX.replaceAllIn(morphPerMessages.text, "")
          morphPerMessages.text = newAmount
          MorphingKeyManager.messagesPerMorph = newAmount.toInt


        case WindowClosing(_) =>
          // Gracefully close the server on WindowClosing
          server.stopServer()
      }
    }
  }

  def connectToClient(host: String = "127.0.0.1", port: Int = 9999, mk: Boolean = true, mr: Boolean, rp: Boolean = false): Unit = {
    // Check if IP is valid
    if (IP_REGEX.findFirstIn(host).isEmpty) {
      Dialog.showMessage(
        null,
        "Invalid IP address entered, please try again. Valid IPs are from 0.0.0.0 to 255.255.255.255!",
        "Error!",
        scala.swing.Dialog.Message.Error
      )
      return
    }

    if (port < 0 || port > 65535) {
      Dialog.showMessage(
        null,
        "Invalid port entered, please try again. Valid ports are between 0 and 65535!",
        "Error!",
        scala.swing.Dialog.Message.Error
      )
      return
    }

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
        "BB84Simulator has run into a problem",
        scala.swing.Dialog.Message.Error
      )
      return
    }

    GUI.main(args) // Initialise the GUI
  }
}
