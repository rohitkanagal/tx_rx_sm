package txrxsm

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

class TxRxSmBlackBox extends Module {
  val io = IO(new Bundle {

    //=============================
    //  Receive (PCS Rx) State Machine I/O
    //=============================

    // -- Inputs --
    val rx_symb_vector            = Input(Vec(4, UInt(3.W)))      // Received vector of quinary symbols
    val pcs_reset                 = Input(Bool())      // PCS Reset control
    val link_status               = Input(Bool())      // Link status from PMA link monitor
    val check_end                 = Input(Bool())      // Detect reception of valid ESD symbols
    val check_idle                = Input(Bool())      // Check for valid idle code-groups after error
    val PMA_RXSTATUS_INDICATE_NOT_OK = Input(Bool())   // PHY’s receive link is unreliable

    // -- Outputs --
    val receive1000BT             = Output(Bool())     // Indicates active 1000BASE-T reception
    val RX_DV                     = Output(Bool())     // GMII: RX Data Valid
    val RX_ER                     = Output(Bool())     // GMII: RX Error
    val RXD                       = Output(UInt(8.W))  // GMII: RX Data Bus
    val rxerror_status            = Output(Bool())     // Error status from PCS Rx


    //=============================
    //  Transmit (PCS Tx) State Machine I/O
    //=============================

    // -- Inputs --
    val tx_enable                 = Input(Bool())      // Enables transmission
    val tx_error                  = Input(Bool())      // Indicates transmit error
    val symb_timer_done           = Input(Bool())      // Symbol timer expires
    val TXD                       = Input(UInt(8.W))   // GMII: TX Data Bus

    // -- Outputs --
    val transmit1000BT            = Output(Bool())     // Indicates active 1000BASE-T transmission
    val COL                       = Output(Bool())     // Collision indication
    val tx_symb_vector            = Output(Vec(4, UInt(3.W)))  // Vector of quinary symbols for transmit
  })

  // The Chisel module name

  // Resource path to the Verilog source
}

