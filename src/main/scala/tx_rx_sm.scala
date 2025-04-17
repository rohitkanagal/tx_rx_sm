package txrxsm

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

// class TxRxStateMachine extends Module {
//   val io = IO(new Bundle {

//     //=============================
//     //  Receive (PCS Rx) State Machine I/O
//     //=============================

//     // -- Inputs --
//     val rx_symb_vector            = Input(Vec(4, UInt(3.W)))      // Received vector of quinary symbols
//     val pcs_reset                 = Input(Bool())      // PCS Reset control
//     val link_status               = Input(Bool())      // Link status from PMA link monitor
//     val check_end                 = Input(Bool())      // Detect reception of valid ESD symbols
//     val check_idle                = Input(Bool())      // Check for valid idle code-groups after error
//     val PMA_RXSTATUS_INDICATE_NOT_OK = Input(Bool())   // PHY’s receive link is unreliable

//     // -- Outputs --
//     val receive1000BT             = Output(Bool())     // Indicates active 1000BASE-T reception
//     val RX_DV                     = Output(Bool())     // GMII: RX Data Valid
//     val RX_ER                     = Output(Bool())     // GMII: RX Error
//     val RXD                       = Output(UInt(8.W))  // GMII: RX Data Bus
//     val rxerror_status            = Output(Bool())     // Error status from PCS Rx


//     //=============================
//     //  Transmit (PCS Tx) State Machine I/O
//     //=============================

//     // -- Inputs --
//     val tx_enable                 = Input(Bool())      // Enables transmission
//     val tx_error                  = Input(Bool())      // Indicates transmit error
//     val symb_timer_done           = Input(Bool())      // Symbol timer expires
//     val TXD                       = Input(UInt(8.W))   // GMII: TX Data Bus

//     // -- Outputs --
//     val transmit1000BT            = Output(Bool())     // Indicates active 1000BASE-T transmission
//     val COL                       = Output(Bool())     // Collision indication
//     val tx_symb_vector            = Output(Vec(4, UInt(3.W)))  // Vector of quinary symbols for transmit
//   })

//   // The Chisel module name

//   // Resource path to the Verilog source
// }

class TxRxStateMachine extends Module {
  val io = IO(new Bundle {
    val pcs_reset                 = Input(Bool())      // PCS Reset control
    val rxerror_status            = Output(Bool())     // Error status from PCS Rx
  })

   io.rxerror_status := io.pcs_reset
}


trait TxRxStateMachineTopIO extends Bundle {}

trait TxRxStateMachineTop extends HasRegMap {
  val io: TxRxStateMachineTopIO
  val clock: Clock
  val reset: Reset

  val pcsReset = RegInit(false.B)


  val machine = withReset(reset) {
    Module(new TxRxStateMachine())
  }

  machine.io.pcs_reset <> pcsReset

  regmap(
    0x00 -> Seq(
      RegField.w(1, pcsReset)
    ),
    0x04 -> Seq(
      RegField.r(1, machine.io.rxerror_status)
    ),
  )
}

class TxRxStateMachineTL(params: TxRxStateMachineParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "rxtxsm", Seq("eecs251b,rxtxsm"),
    beatBytes = beatBytes)(
      new TLRegBundle(params, _) with TxRxStateMachineTopIO)(
      new TLRegModule(params, _, _) with TxRxStateMachineTop)


case class TxRxStateMachineParams(
  address: BigInt = 0x4000,
)

case object TxRxStateMachineKey extends Field[Option[TxRxStateMachineParams]](None)

trait CanHavePeripheryTxRxStateMachine { this: BaseSubsystem =>
  private val portName = "txrxsm"

  val txrxsm = p(TxRxStateMachineKey) match {
    case Some(params) => {
      val txrxsm = LazyModule(new TxRxStateMachineTL(params, pbus.beatBytes)(p))
      pbus.coupleTo(portName) { txrxsm.node := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
      Some(txrxsm)
    }
    case None => None
  }
}

trait CanHavePeripheryTxRxStateMachineImp extends LazyModuleImp {
  val outer: CanHavePeripheryTxRxStateMachine
}


class WithTxRxStateMachine(params: TxRxStateMachineParams) extends Config((site, here, up) => {
  case TxRxStateMachineKey => Some(params)
})