package txrxsm

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import PcsCodeGroups._

object PcsRxState extends ChiselEnum {
  val IDLE,
      NON_IDLE_DETECT,
      CONFIRM,
      SSD1_VECTOR,
      SSD2_VECTOR,
      BAD_SSD,
      RECEIVE,
      CSReset,
      ESD1,
      ESD2 = Value
}

object PcsTxState extends ChiselEnum {
  val IDLE,
      SSD1_VECTOR,
      SSD2_VECTOR,
      SSD1_VECTOR_ERROR,
      SSD2_VECTOR_ERROR,
      TRANSMIT_DATA,
      TRANSMIT_ERROR,
      CSReset1,
      CSReset2,
      ESD1,
      ESD2 = Value
}

object PcsCodeGroups {

  /* ------------------------------------------------------------------
   *  Helper to build Vec(4,SInt(3.W)) from 4 Scala Ints (+2,+1,0,-1,-2)
   * ------------------------------------------------------------------ */
   def V(a: Int, b: Int, c: Int, d: Int): Vec[SInt] =
    VecInit(a.S(3.W), b.S(3.W), c.S(3.W), d.S(3.W))

  /* ----------- Table 40‑1 / 40‑2 code‑group patterns ---------------- */

  // Delimiters
  def SSD1       = V(+2, +2, +2, +2)
  def SSD2       = V(+2, +2, +2, -2)
  def ESD1       = V(+2, +2, +2, +2)
  def ESD2_0     = V(+2, +2, +2, -2)
  def ESD2_1     = V(+2, +2, -2, +2)
  def ESD2_2     = V(+2, -2, +2, +2)
  def ESD_ERR    = V(-2, +2, +2, +2)
  
  // Carrier-extension (half-duplex)
  def CEXT       = V(+2,  0,  0, +2)
  def CEXT_ERR   = V(-2, +2, +2, +2)

   def XMT_ERR: Seq[Vec[SInt]] = Seq(
    V(0,+2,+2,0), V(+1,+1,+2,+2), V(+2,+1,+1,+2), V(+2,+1,+2,+1), 
    V(0,+2,+1,+2), V(+1,+2,+2,0), V(+2,+1,+2,0)
  )

  def CSEXTEND_ERR: Seq[Vec[SInt]] = Seq(
    V(-2,+2,+2,-2), V(-1,-1,+2,+2), V(+2,-1,-1,+2), V(+2,-1,+2,-1), 
    V(-2,+2,-1,+2), V(-1,+2,+2,-2), V(+2,-1,+2,-2)
  )

  def CSEXTEND: Seq[Vec[SInt]] = Seq(
    V(+2,+2,+1,+1), V(+1,+2,+2,+1), V(+1,+2,+1,+2)
  )

  def CSRESET: Seq[Vec[SInt]] = Seq(
    V(+2,-2,-2,+2), V(+2,+2,-1,-1), V(-1,+2,+2,-1), V(-1,+2,-1,+2), 
    V(+2,-2,+2,-1), V(+2,-2,-1,+2), V(-1,-2,+2,+2), V(+2,-1,-2,+2)
  )

  def ESD2_Ext_0 = V(+2, +2, +2, -2)  // Valid ESD2 for CEXT=0
  def ESD2_Ext_1 = V(+2, +2, -2, +2)
  def ESD2_Ext_2 = V(+2, -2, +2, +2)

  def ValidDataGroups: Seq[Vec[SInt]] = Seq(
    V(0, 0, 0, 0), V(0, 0, +1, +1), V(0, +1, +1, 0), V(0, +1, 0, +1),
V(-2, 0, 0, 0), V(-2, 0, +1, +1), V(-2, +1, +1, 0), V(-2, +1, 0, +1),
V(0, -2, 0, 0), V(0, -2, +1, +1), V(0, -1, +1, 0), V(0, -1, 0, +1),
V(-2, -2, 0, 0), V(-2, -2, +1, +1), V(-2, -1, +1, 0), V(-2, -1, 0, +1),
V(0, 0, -2, 0), V(0, 0, -1, +1), V(0, +1, -1, 0), V(0, +1, -2, +1),
V(-2, 0, -2, 0), V(-2, 0, -1, +1), V(-2, +1, -1, 0), V(-2, +1, -2, +1),
V(0, -2, -2, 0), V(0, -2, -1, +1), V(0, -1, -1, 0), V(0, -1, -2, +1),
V(-2, -2, -2, 0), V(-2, -2, -1, +1), V(-2, -1, -1, 0), V(-2, -1, -2, +1),
V(0, 0, 0, -2), V(0, 0, +1, -1), V(0, +1, +1, -2), V(0, +1, 0, -1),
V(-2, 0, 0, -2), V(-2, 0, +1, -1), V(-2, +1, +1, -2), V(-2, +1, 0, -1),
V(0, -2, 0, -2), V(0, -2, +1, -1), V(0, -1, +1, -2), V(0, -1, 0, -1),
V(-2, -2, 0, -2), V(-2, -2, +1, -1), V(-2, -1, +1, -2), V(-2, -1, 0, -1),
V(0, 0, -2, -2), V(0, 0, -1, -1), V(0, +1, -1, -2), V(0, +1, -2, -1),
V(-2, 0, -2, -2), V(-2, 0, -1, -1), V(-2, +1, -1, -2), V(-2, +1, -2, -1),
V(0, -2, -2, -2), V(0, -2, -1, -1), V(0, -1, -1, -2), V(0, -1, -2, -1),
V(-2, -2, -2, -2), V(-2, -2, -1, -1), V(-2, -1, -1, -2), V(-2, -1, -2, -1),
V(+1, +1, +1, +1), V(+1, +1, 0, 0), V(+1, 0, 0, +1), V(+1, 0, +1, 0),
V(-1, +1, +1, +1), V(-1, +1, 0, 0), V(-1, 0, 0, +1), V(-1, 0, +1, 0),
V(+1, -1, +1, +1), V(+1, -1, 0, 0), V(+1, -2, 0, +1), V(+1, -2, +1, 0),
V(-1, -1, +1, +1), V(-1, -1, 0, 0), V(-1, -2, 0, +1), V(-1, -2, +1, 0),
V(+1, +1, -1, +1), V(+1, +1, -2, 0), V(+1, 0, -2, +1), V(+1, 0, -1, 0),
V(-1, +1, -1, +1), V(-1, +1, -2, 0), V(-1, 0, -2, +1), V(-1, 0, -1, 0),
V(+1, -1, -1, +1), V(+1, -1, -2, 0), V(+1, -2, -2, +1), V(+1, -2, -1, 0),
V(-1, -1, -1, +1), V(-1, -1, -2, 0), V(-1, -2, -2, +1), V(-1, -2, -1, 0),
V(+1, +1, +1, -1), V(+1, +1, 0, -2), V(+1, 0, 0, -1), V(+1, 0, +1, -2),
V(-1, +1, +1, -1), V(-1, +1, 0, -2), V(-1, 0, 0, -1), V(-1, 0, +1, -2),
V(+1, -1, +1, -1), V(+1, -1, 0, -2), V(+1, -2, 0, -1), V(+1, -2, +1, -2),
V(-1, -1, +1, -1), V(-1, -1, 0, -2), V(-1, -2, 0, -1), V(-1, -2, +1, -2),
V(+1, +1, -1, -1), V(+1, +1, -2, -2), V(+1, 0, -2, -1), V(+1, 0, -1, -2),
V(-1, +1, -1, -1), V(-1, +1, -2, -2), V(-1, 0, -2, -1), V(-1, 0, -1, -2),
V(+1, -1, -1, -1), V(+1, -1, -2, -2), V(+1, -2, -2, -1), V(+1, -2, -1, -2),
V(-1, -1, -1, -1), V(-1, -1, -2, -2), V(-1, -2, -2, -1), V(-1, -2, -1, -2),
V(+2, 0, 0, 0), V(+2, 0, +1, +1), V(+2, +1, +1, 0), V(+2, +1, 0, +1),
V(+2, -2, 0, 0), V(+2, -2, +1, +1), V(+2, -1, +1, 0), V(+2, -1, 0, +1),
V(+2, 0, -2, 0), V(+2, 0, -1, +1), V(+2, +1, -1, 0), V(+2, +1, -2, +1),
V(+2, -2, -2, 0), V(+2, -2, -1, +1), V(+2, -1, -1, 0), V(+2, -1, -2, +1),
V(+2, 0, 0, -2), V(+2, 0, +1, -1), V(+2, +1, +1, -2), V(+2, +1, 0, -1),
V(+2, -2, 0, -2), V(+2, -2, +1, -1), V(+2, -1, +1, -2), V(+2, -1, 0, -1),
V(+2, 0, -2, -2), V(+2, 0, -1, -1), V(+2, +1, -1, -2), V(+2, +1, -2, -1),
V(+2, -2, -2, -2), V(+2, -2, -1, -1), V(+2, -1, -1, -2), V(+2, -1, -2, -1),
V(0, 0, +2, 0), V(+1, +1, +2, 0), V(+1, 0, +2, +1), V(0, +1, +2, +1),
V(-2, 0, +2, 0), V(-1, +1, +2, 0), V(-1, 0, +2, +1), V(-2, +1, +2, +1),
V(0, -2, +2, 0), V(+1, -1, +2, 0), V(+1, -2, +2, +1), V(0, -1, +2, +1),
V(-2, -2, +2, 0), V(-1, -1, +2, 0), V(-1, -2, +2, +1), V(-2, -1, +2, +1),
V(0, 0, +2, -2), V(+1, +1, +2, -2), V(+1, 0, +2, -1), V(0, +1, +2, -1),
V(-2, 0, +2, -2), V(-1, +1, +2, -2), V(-1, 0, +2, -1), V(-2, +1, +2, -1),
V(0, -2, +2, -2), V(+1, -1, +2, -2), V(+1, -2, +2, -1), V(0, -1, +2, -1),
V(-2, -2, +2, -2), V(-1, -1, +2, -2), V(-1, -2, +2, -1), V(-2, -1, +2, -1),
V(0, +2, 0, 0), V(0, +2, +1, +1), V(+1, +2, 0, +1), V(+1, +2, +1, 0),
V(-2, +2, 0, 0), V(-2, +2, +1, +1), V(-1, +2, 0, +1), V(-1, +2, +1, 0),
V(0, +2, -2, 0), V(0, +2, -1, +1), V(+1, +2, -2, +1), V(+1, +2, -1, 0),
V(-2, +2, -2, 0), V(-2, +2, -1, +1), V(-1, +2, -2, +1), V(-1, +2, -1, 0),
V(0, +2, 0, -2), V(0, +2, +1, -1), V(+1, +2, 0, -1), V(+1, +2, +1, -2),
V(-2, +2, 0, -2), V(-2, +2, +1, -1), V(-1, +2, 0, -1), V(-1, +2, +1, -2),
V(0, +2, -2, -2), V(0, +2, -1, -1), V(+1, +2, -2, -1), V(+1, +2, -1, -2),
V(-2, +2, -2, -2), V(-2, +2, -1, -1), V(-1, +2, -2, -1), V(-1, +2, -1, -2),
V(0, 0, 0, +2), V(+1, +1, 0, +2), V(0, +1, +1, +2), V(+1, 0, +1, +2),
V(-2, 0, 0, +2), V(-1, +1, 0, +2), V(-2, +1, +1, +2), V(-1, 0, +1, +2),
V(0, -2, 0, +2), V(+1, -1, 0, +2), V(0, -1, +1, +2), V(+1, -2, +1, +2),
V(-2, -2, 0, +2), V(-1, -1, 0, +2), V(-2, -1, +1, +2), V(-1, -2, +1, +2),
V(0, 0, -2, +2), V(+1, +1, -2, +2), V(0, +1, -1, +2), V(+1, 0, -1, +2),
V(-2, 0, -2, +2), V(-1, +1, -2, +2), V(-2, +1, -1, +2), V(-1, 0, -1, +2),
V(0, -2, -2, +2), V(+1, -1, -2, +2), V(0, -1, -1, +2), V(+1, -2, -1, +2),
V(-2, -2, -2, +2), V(-1, -1, -2, +2), V(-2, -1, -1, +2), V(-1, -2, -1, +2)
  )

  /* ------------------ Match helpers ------------------ */

  def eq4(a: Vec[SInt], b: Vec[SInt]): Bool =
    a.zip(b).map { case (x, y) => x === y }.reduce(_ && _)

  def isSSD1(v: Vec[SInt]): Bool       = eq4(v, SSD1)
  def isSSD2(v: Vec[SInt]): Bool       = eq4(v, SSD2)
  def isESD1(v: Vec[SInt]): Bool       = eq4(v, ESD1)

  def isXmtErr(v: Vec[SInt]): Bool =
    XMT_ERR.map(g => eq4(v, g)).reduce(_ || _)

  def isCSExtendErr(v: Vec[SInt]): Bool =
    CSEXTEND_ERR.map(g => eq4(v, g)).reduce(_ || _)

  def isCSExtend(v: Vec[SInt]): Bool =
    CSEXTEND.map(g => eq4(v, g)).reduce(_ || _)

  def isCSReset(v: Vec[SInt]): Bool =
    CSRESET.map(g => eq4(v, g)).reduce(_ || _)

  def isESD2(v: Vec[SInt]): Bool =
    eq4(v, ESD2_0) || eq4(v, ESD2_1) || eq4(v, ESD2_2)

  def isESD2Ext0(v: Vec[SInt]): Bool = eq4(v, ESD2_Ext_0)
  def isESD2Ext1(v: Vec[SInt]): Bool = eq4(v, ESD2_Ext_1)
  def isESD2Ext2(v: Vec[SInt]): Bool = eq4(v, ESD2_Ext_2)

  def isCext(v: Vec[SInt]): Bool = eq4(v, CEXT)
  def isCextErr(v: Vec[SInt]): Bool = eq4(v, CEXT_ERR)

  def isEsdErr(v: Vec[SInt]): Bool = eq4(v, ESD_ERR)

  def isIdle(v: Vec[SInt]): Bool =
    v.map(symb => (symb === -2.S) || (symb === 0.S)).reduce(_ && _)

  def isDataCodeGroup(v: Vec[SInt]): Bool =
    ValidDataGroups.map(g => eq4(v, g)).reduce(_ || _)
}

class TxRxStateMachine extends Module {
  val io = IO(new Bundle {
    val rx_symb_vector            = Flipped(Decoupled(Vec(4, SInt(3.W))))
    val pcs_reset                 = Input(Bool())      // PCS Reset control
    val decoded_rx_symb_vector    = Input(UInt(8.W))   // From Decoder

    val rxd                       = Output(UInt(8.W))  // GMII: RX Data Bus
    val rx_dv                     = Output(Bool())     // GMII: RX Data Valid
    val rx_er                     = Output(Bool())     // GMII: RX Data Error
    val rxerror_status            = Output(Bool())     // Error status from PCS Rx

    val tx_enable                 = Input(Bool())      // Enables transmission
    val tx_error                  = Input(Bool())      // Indicates transmit error
    val symb_timer_done           = Input(Bool())      // Symbol timer expires
    val txd                       = Input(UInt(8.W))   // GMII: TX Data Bus
    val encoded_tx_symb_vector    = Input(Vec(4, SInt(3.W)))              // From Encoder

    val col                       = Output(Bool())                        // Collision indication
    val tx_symb_vector            = Decoupled(Vec(4, SInt(3.W)))          // Vector of quinary symbols for transmit

  })
  // TX STATE MACHINE
   io.tx_symb_vector.noenq()
   io.col := false.B

   val _1000BTtransmit = WireDefault(false.B)
   val _1000BTreceive = WireDefault(false.B)

   val PUDR        = io.tx_symb_vector.fire

   val txstate = RegInit(PcsTxState.IDLE)

   switch (txstate) {
    is (PcsTxState.IDLE) {
      _1000BTtransmit := false.B
      io.col := false.B
      io.tx_symb_vector.enq(V(-2, -2, -2, -2))

      when (!io.tx_enable && PUDR) {
        txstate := PcsTxState.IDLE
      }.elsewhen (io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.SSD1_VECTOR
      }.elsewhen (io.tx_enable && io.tx_error && PUDR) {
        txstate := PcsTxState.SSD1_VECTOR_ERROR
      }.otherwise{
        txstate := PcsTxState.IDLE
      }
    }
    is (PcsTxState.SSD1_VECTOR) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(V(+2, +2, +2, +2))

      when (!io.tx_error && PUDR) {
        txstate := PcsTxState.SSD1_VECTOR
      }.elsewhen (io.tx_error && PUDR) {
        txstate := PcsTxState.SSD2_VECTOR_ERROR
      }.otherwise{
        txstate := PcsTxState.SSD1_VECTOR
      }
    }

    is (PcsTxState.SSD2_VECTOR) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(V(+2, +2, +2, -2))

      when (io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_DATA
      }.elsewhen (!io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.CSReset1
      }.elsewhen (io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_ERROR
      }.otherwise{
        txstate := PcsTxState.SSD2_VECTOR
      }
    }
    

    is (PcsTxState.SSD1_VECTOR_ERROR) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(V(+2, +2, +2, +2))

      when (PUDR) {
        txstate := PcsTxState.SSD2_VECTOR_ERROR
      }.otherwise{
        txstate := PcsTxState.SSD1_VECTOR_ERROR
      }
    }

    is (PcsTxState.SSD2_VECTOR_ERROR) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(V(+2, +2, +2, +2))

      when (PUDR) {
        txstate := PcsTxState.TRANSMIT_ERROR
      }.otherwise{
        txstate := PcsTxState.SSD2_VECTOR_ERROR
      }
    }

    is (PcsTxState.TRANSMIT_DATA) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(io.encoded_tx_symb_vector)

      when (io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_DATA
      }.elsewhen (!io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.CSReset1
      }.elsewhen (io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_ERROR
      }.otherwise{
        txstate := PcsTxState.TRANSMIT_DATA
      }
    }
    is (PcsTxState.TRANSMIT_ERROR) {
      _1000BTtransmit := true.B
      io.col := _1000BTreceive
      io.tx_symb_vector.enq(V(0,+2,+2,0))

      when (io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_DATA
      }.elsewhen (!io.tx_enable && !io.tx_error && PUDR) {
        txstate := PcsTxState.CSReset1
      }.elsewhen (io.tx_error && PUDR) {
        txstate := PcsTxState.TRANSMIT_ERROR
      }.otherwise{
        txstate := PcsTxState.TRANSMIT_ERROR
      }
    }

    is (PcsTxState.CSReset1) {
      _1000BTtransmit := false.B
      io.col := false.B
      io.tx_symb_vector.enq(V(+2,-2,-2,+2))

      when (PUDR) {
        txstate := PcsTxState.CSReset2
      }.otherwise{
        txstate := PcsTxState.CSReset1
      }
    }

    is (PcsTxState.CSReset2) {
      _1000BTtransmit := false.B
      io.col := false.B
      io.tx_symb_vector.enq(V(+2,-2,-2,+2))

      when (PUDR) {
        txstate := PcsTxState.ESD1
      }.otherwise{
        txstate := PcsTxState.CSReset2
      }
    }

    is (PcsTxState.ESD1) {
      _1000BTtransmit := false.B
      io.col := false.B
      io.tx_symb_vector.enq(V(+2, +2, +2, +2))

      when (PUDR) {
        txstate := PcsTxState.ESD2
      }.otherwise{
        txstate := PcsTxState.ESD1
      }
    }

    is (PcsTxState.ESD2) {
      _1000BTtransmit := false.B
      io.col := false.B
      io.tx_symb_vector.enq(V(+2, +2, +2, -2) )

      when (PUDR) {
        txstate := PcsTxState.IDLE
      }.otherwise{
        txstate := PcsTxState.ESD2
      }
    }


   }

  // RX STATE MACHINE
   io.rx_symb_vector.nodeq()
   io.rxd := RegInit(0.U(8.W)) // Wire(UInt())
   io.rx_dv := false.B
   io.rx_er := false.B
   io.rxerror_status := false.B

   val rxHist = ShiftRegisters(io.rx_symb_vector.bits, 3, io.rx_symb_vector.fire) // rx_hist(0) = rx_n, rx_hist(1) = rx_n-1, etc.
   val Rx_n = io.rx_symb_vector.bits          // incoming
   val Rx_n_minus1 = rxHist(0)                // 1‑cycle old
   val Rx_n_minus2 = rxHist(1)                // 2‑cycle old
   val Rx_n_minus3 = rxHist(2)                // 3‑cycle old
   val PUDI        = io.rx_symb_vector.fire

   val state = RegInit(PcsRxState.IDLE)

  switch (state) {
    is (PcsRxState.IDLE) {
      io.rx_symb_vector.ready := true.B
      _1000BTreceive := false.B
      io.rxerror_status := false.B
      io.rx_dv := false.B
      io.rx_er := false.B
      io.rxd := "h00".U

      when (!isIdle(Rx_n) && PUDI) {
        state := PcsRxState.NON_IDLE_DETECT
      }.otherwise{
        state := PcsRxState.IDLE
      }
    }
    is (PcsRxState.NON_IDLE_DETECT) { 
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := true.B
        io.rxerror_status := false.B
        io.rx_dv := false.B
        io.rx_er := false.B
        io.rxd := "h00".U

        when (PUDI && isSSD1(Rx_n_minus1) && isSSD2(Rx_n)) {
          state := PcsRxState.SSD1_VECTOR
        }.elsewhen(PUDI && !(isSSD1(Rx_n_minus1) && isSSD2(Rx_n))) {
          state := PcsRxState.BAD_SSD
        }.otherwise{
          state := PcsRxState.NON_IDLE_DETECT
        }
      }
      is (PcsRxState.BAD_SSD) {
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := true.B
        io.rxerror_status := true.B
        io.rx_dv := false.B
        io.rx_er := true.B
        io.rxd := "h0E".U


        when (PUDI && isIdle(Rx_n)) {
          state := PcsRxState.IDLE
        }.otherwise{
          state := PcsRxState.BAD_SSD
        }
      }
      is (PcsRxState.SSD1_VECTOR) {
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := true.B
        io.rxerror_status := false.B
        io.rx_dv := true.B
        io.rx_er := false.B
        io.rxd := "h55".U

        when (PUDI) {
          state := PcsRxState.SSD2_VECTOR
        }.otherwise{
          state := PcsRxState.SSD1_VECTOR
        }
      }
      is (PcsRxState.SSD2_VECTOR) {
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := true.B
        io.rxerror_status := false.B
        io.rx_dv := true.B
        io.rx_er := false.B
        io.rxd := "h55".U

        when (PUDI) {
          state := PcsRxState.RECEIVE
        }.otherwise{
          state := PcsRxState.SSD2_VECTOR
        }
      }
      is (PcsRxState.RECEIVE) {
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := true.B
        io.rxerror_status := false.B
        io.rx_dv := true.B
        io.rx_er := false.B
        io.rxd := io.decoded_rx_symb_vector

        when (PUDI && isDataCodeGroup(Rx_n)) {
          state := PcsRxState.RECEIVE
          io.rx_er := false.B
        }.elsewhen (PUDI && isIdle(Rx_n)) {
          io.rx_er := true.B
          state := PcsRxState.IDLE
        }.elsewhen (PUDI && isCSReset(Rx_n)) {
          io.rx_er := false.B
          io.rx_dv := false.B
          _1000BTreceive := false.B
          state := PcsRxState.CSReset
        }.elsewhen (PUDI && isXmtErr(Rx_n)) {
          io.rx_er := true.B
          state := PcsRxState.RECEIVE
        }.otherwise{
          state := PcsRxState.RECEIVE
        }
      }
      is (PcsRxState.CSReset) {
        io.rx_symb_vector.ready := true.B

        _1000BTreceive := false.B
        io.rxerror_status := false.B
        io.rx_dv := false.B
        io.rx_er := false.B
        io.rxd := io.decoded_rx_symb_vector

        when (PUDI && isCSReset(Rx_n)) {
          state := PcsRxState.ESD1
        }.otherwise{
          state := PcsRxState.CSReset
        }
      }
      is (PcsRxState.ESD1) {
        io.rx_symb_vector.ready := true.B
        
        _1000BTreceive := false.B
        io.rxerror_status := false.B
        io.rx_dv := false.B
        io.rx_er := false.B
        io.rxd := io.decoded_rx_symb_vector

        when (PUDI && isESD1(Rx_n)) {
          state := PcsRxState.ESD2
        }.otherwise{
          state := PcsRxState.ESD1
        }
      }
      is (PcsRxState.ESD2) { //maybe can be removed
        io.rx_symb_vector.ready := true.B
        
        _1000BTreceive := false.B
        io.rxerror_status := false.B
        io.rx_dv := false.B
        io.rx_er := false.B
        io.rxd := io.decoded_rx_symb_vector

        when (PUDI && isESD2(Rx_n)) {
          state := PcsRxState.IDLE
        }.otherwise{
          state := PcsRxState.ESD2
        }
      }
  }


  


}


trait TxRxStateMachineTopIO extends Bundle {}

trait TxRxStateMachineTop extends HasRegMap {
  val io: TxRxStateMachineTopIO
  val clock: Clock
  val reset: Reset

  val reg_pcsReset               = RegInit(false.B)
  val reg_txEnable               = RegInit(false.B)
  val reg_txError                = RegInit(false.B)
  val reg_symbTimerDone          = RegInit(false.B)
  val reg_txd                    = RegInit(0.U(8.W))
  val reg_decodedRxSymbVector    = RegInit(0.U(8.W))
  val reg_encodedTxSymbVector    = RegInit(0.U(12.W)) 
  val reg_RxSymbVector           = RegInit(0.U(12.W)) 


  val machine = withReset(reset) {
    Module(new TxRxStateMachine())
  }

  machine.io.pcs_reset <> reg_pcsReset
  machine.io.tx_enable <> reg_txEnable
  machine.io.tx_error  <> reg_txError
  machine.io.symb_timer_done <> reg_symbTimerDone
  machine.io.txd <> reg_txd
  machine.io.decoded_rx_symb_vector <> reg_decodedRxSymbVector
  machine.io.encoded_tx_symb_vector <> VecInit(
    reg_encodedTxSymbVector.asBools.grouped(3).map { bits =>
      Cat(bits.reverse).asSInt
    }.toSeq
  )
  machine.io.rx_symb_vector.enq(
  VecInit(
    reg_RxSymbVector.asBools.grouped(3).map { bits =>
      Cat(bits.reverse).asSInt
    }.toSeq
  )
)
  machine.io.tx_symb_vector.ready <> true.B

   regmap(
    0x00 -> Seq(RegField.w(1, reg_pcsReset)),                         // PCS reset
    0x04 -> Seq(RegField.w(1, reg_txEnable)),             // TX enable
    0x08 -> Seq(RegField.w(1, reg_txError)),              // TX error
    0x0C -> Seq(RegField.w(1, reg_symbTimerDone)),       // Symbol timer done
    0x10 -> Seq(RegField.w(8, reg_txd)),                   // TXD data bus
    0x14 -> Seq(RegField.w(8, reg_decodedRxSymbVector)),// Decoded RXD bus (8b from decoder)
    0x18 -> Seq(RegField.w(12, reg_encodedTxSymbVector)), // Encoded TX vector (4 x 3 bits = 12b)
    0x1C -> Seq(RegField.r(8, machine.io.rxd)),                   // RXD (GMII)
    0x20 -> Seq(RegField.r(1, machine.io.rx_dv)),                 // RX data valid
    0x24 -> Seq(RegField.r(1, machine.io.rx_er)),                 // RX data error
    0x28 -> Seq(RegField.r(1, machine.io.rxerror_status)),        // PCS RX error status
    0x2C -> Seq(RegField.r(1, machine.io.col)),                    // Collision detect
    0x30 -> Seq(RegField.r(12, machine.io.tx_symb_vector.bits.asUInt)), // TX symb vector output (4 x 3b = 12b)
    0x34 -> Seq(RegField.w(12, reg_RxSymbVector)) // TX symb vector output (4 x 3b = 12b)


  )

//   regmap(
//   // ---- Inputs (write) ----
//   0x00 -> Seq(RegField.w(1, pcsReset)),                         // PCS reset
//   0x04 -> Seq(RegField.w(1, machine.io.tx_enable)),             // TX enable
//   0x08 -> Seq(RegField.w(1, machine.io.tx_error)),              // TX error
//   0x0C -> Seq(RegField.w(1, machine.io.symb_timer_done)),       // Symbol timer done
//   0x10 -> Seq(RegField.w(8, machine.io.txd)),                   // TXD data bus
//   0x14 -> Seq(RegField.w(8, machine.io.decoded_rx_symb_vector)),// Decoded RXD bus (8b from decoder)
//   0x18 -> Seq(RegField.w(12, machine.io.encoded_tx_symb_vector.asUInt)), // Encoded TX vector (4 x 3 bits = 12b)

//   // ---- Outputs (read) ----
//   0x1C -> Seq(RegField.r(8, machine.io.rxd)),                   // RXD (GMII)
//   0x20 -> Seq(RegField.r(1, machine.io.rx_dv)),                 // RX data valid
//   0x24 -> Seq(RegField.r(1, machine.io.rx_er)),                 // RX data error
//   0x28 -> Seq(RegField.r(1, machine.io.rxerror_status)),        // PCS RX error status
//   0x2C -> Seq(RegField.r(1, machine.io.col))                    // Collision detect
//   0x30 -> Seq(RegField.wr(12, machine.io.tx_symb_vector.bits.asUInt)) // TX symb vector output (4 x 3b = 12b)
// )


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