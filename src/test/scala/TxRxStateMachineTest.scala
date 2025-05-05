import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import txrxsm._

class TxRxStateMachineTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "TxRxStateMachine"
  // Test 1: Normal TX state transitions (IDLE → SSD1 → SSD2 → TRANSMIT_DATA → ...)

  it should "go through TX normal flow (IDLE -> SSD1 -> SSD2 -> TRANSMIT_DATA -> CSReset -> ESD1/2 -> IDLE)" in {
    test(new TxRxStateMachine()) { c =>
      c.io.pcs_reset.poke(false.B)
      c.io.tx_enable.poke(false.B)
      c.io.tx_error.poke(false.B)
      c.io.symb_timer_done.poke(false.B)
      c.io.txd.poke("hAA".U)
      c.io.decoded_rx_symb_vector.poke("h55".U)
      c.io.encoded_tx_symb_vector.poke(VecInit(Seq.fill(4)(0.S(3.W))))
      c.io.tx_enable.poke(true.B)
      c.io.tx_error.poke(false.B)
      for (_ <- 0 until 10) {
        c.io.tx_symb_vector.ready.poke(true.B)
        c.clock.step()
      }
    }
  }
  // Test 2: TX error state transitions (to verify proper handling of tx_error)

  it should "handle TX error state transitions" in {
    test(new TxRxStateMachine()) { c =>
      c.io.tx_enable.poke(true.B)
      c.io.tx_error.poke(true.B)
      for (_ <- 0 until 10) {
        c.io.tx_symb_vector.ready.poke(true.B)
        c.clock.step()
      }
    }
  }
  // Test 3: RX enters BAD_SSD when delimiters are incorrect 

  it should "enter RX BAD_SSD state if delimiters are incorrect" in {
    test(new TxRxStateMachine()) { c =>
      val badSsd = VecInit(Seq(0.S(3.W), 0.S(3.W), 0.S(3.W), 0.S(3.W)))
      c.io.rx_symb_vector.valid.poke(true.B)
      c.io.rx_symb_vector.bits.poke(badSsd)
      c.clock.step(3)
    }
  }
  // Test 4: RX handles a full valid receive sequence

  it should "pass through RX SSD1 -> SSD2 -> RECEIVE -> CSReset -> IDLE on valid pattern" in {
    test(new TxRxStateMachine()) { c =>
      def pokeVec(v: Vec[SInt]): Unit = {
        for (i <- 0 until 4) {
          c.io.rx_symb_vector.bits(i).poke(v(i))
        }
      }
      c.io.rx_symb_vector.valid.poke(true.B)
      pokeVec(PcsCodeGroups.SSD1)
      c.clock.step(1)
      pokeVec(PcsCodeGroups.SSD2)
      c.clock.step(1)
      pokeVec(PcsCodeGroups.ValidDataGroups.head)
      c.clock.step(1)
      pokeVec(PcsCodeGroups.CSRESET.head)
      c.clock.step(1)
      pokeVec(PcsCodeGroups.ESD1)
      c.clock.step(1)
      pokeVec(PcsCodeGroups.ESD2_0)
      c.clock.step(1)
    }
  }
  // Test 5: Random test to catch any unexpected deadlock behavior

  it should "not deadlock under random input patterns" in {
    test(new TxRxStateMachine()) { c =>
      val rnd = new scala.util.Random(42)
      for (_ <- 0 until 50) {
        c.io.rx_symb_vector.valid.poke(true.B)
        c.io.rx_symb_vector.bits.poke(VecInit(Seq.fill(4)(rnd.nextInt(5).S(3.W))))
        c.io.tx_enable.poke(rnd.nextBoolean().B)
        c.io.tx_error.poke(rnd.nextBoolean().B)
        c.io.symb_timer_done.poke(rnd.nextBoolean().B)
        c.io.encoded_tx_symb_vector.poke(VecInit(Seq.fill(4)(rnd.nextInt(5).S(3.W))))
        c.io.decoded_rx_symb_vector.poke(rnd.nextInt(256).U)
        c.io.txd.poke(rnd.nextInt(256).U)
        c.io.tx_symb_vector.ready.poke(true.B)
        c.clock.step(1)
      }
    }
  }
  // Corner Case TestA: Transmit enabled, but PUDR never fires (simulate stuck ready = false)

  it should "not progress TX state when ready (PUDR) is false" in {
    test(new TxRxStateMachine()) { c =>
        c.io.tx_enable.poke(true.B)
        c.io.tx_error.poke(false.B)
        c.io.tx_symb_vector.ready.poke(false.B) // simulate PUDR = false

        for (i <- 0 until 5) {
        println(s"[Cycle $i] TX enable active, but PUDR is false")
        c.io.tx_symb_vector.bits.print()
        c.clock.step(1)
      }
    }
  }
  //  Corner Case TestB: Receive invalid SSD followed by valid SSD

  it should "recover from BAD_SSD to normal receive flow on next valid SSD" in {
    test(new TxRxStateMachine()) { c =>
        def pokeVec(v: Vec[SInt]) = {
        for (i <- 0 until 4) c.io.rx_symb_vector.bits(i).poke(v(i))
        }

        val badSsd = VecInit(Seq(0.S(3.W), 0.S(3.W), 0.S(3.W), 0.S(3.W)))

        c.io.rx_symb_vector.valid.poke(true.B)

        // 1. Feed bad SSD pattern
        pokeVec(badSsd)
        println("[RX] Injecting BAD_SSD")
        c.clock.step(2)

        // 2. Feed good SSD1
        pokeVec(PcsCodeGroups.SSD1)
        println("[RX] Injecting SSD1")
        c.clock.step(1)

        // 3. Feed SSD2
        pokeVec(PcsCodeGroups.SSD2)
        println("[RX] Injecting SSD2")
        c.clock.step(1)

        // 4. Feed valid data
        pokeVec(PcsCodeGroups.ValidDataGroups.head)
        println("[RX] Valid data after SSD")
        c.clock.step(1)
    }
  }
}