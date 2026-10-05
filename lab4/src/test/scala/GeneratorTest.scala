import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._
import chisel3.util.HasBlackBoxPath

class GeneratorTest extends AnyFlatSpec with ChiselScalatestTester {

  val enableWaveform = true


  val annotations = if (enableWaveform) {
    Seq(WriteVcdAnnotation)
  } else {
    Seq()
  }

  "Chisel CSR Generator" should "generate CSR adapter for soc.xlsx" in {
    // test that the PythonSocAdapter blackbox can be instantiated
    test(new CsrAdapter("soc.xlsx")).withAnnotations(annotations) { dut =>
      val bfm = new ApbMasterBfm(
        dut.clock,
        dut.reset,
        dut.apb.psel,
        dut.apb.penable,
        dut.apb.paddr,
        dut.apb.pwrite,
        dut.apb.pwdata,
        dut.apb.prdata,
        dut.apb.pready,
        dut.apb.pslverr
      )

      // reset DUT
      bfm.reset()

      bfm.readExpect(0x80000000L, Some(0xdeadbeefL)) // constant
    }
  }

  /** Run `body` against a fresh Chisel adapter generated from soc.xlsx, after reset. */
  def withDut(body: (CsrAdapter, ApbMasterBfm) => Unit): Unit =
    test(new CsrAdapter("soc.xlsx")).withAnnotations(annotations) { dut =>
      val bfm = new ApbMasterBfm(
        dut.clock,
        dut.reset,
        dut.apb.psel,
        dut.apb.penable,
        dut.apb.paddr,
        dut.apb.pwrite,
        dut.apb.pwdata,
        dut.apb.prdata,
        dut.apb.pready,
        dut.apb.pslverr
      )
      bfm.reset()
      body(dut, bfm)
    }

  /** Fields of the generated CSR bundle are untyped `Data`. */
  def u(d: Data): UInt = d.asInstanceOf[UInt]

  "CsrAdapter" should "read a const register" in withDut { (dut, bfm) =>
    bfm.readExpect(0x80000000L, Some(0xdeadbeefL))
  }

  it should "reflect a ro input on the bus" in withDut { (dut, bfm) =>
    u(dut.csr("gpio0.dataIn")).poke(0xdeadbeefL.U)
    bfm.readExpect(0x41004008L, Some(0xdeadbeefL))
  }

  it should "reset rw fields to init and mask writes to the field range" in withDut { (dut, bfm) =>
    bfm.readExpect(0x41003000L, Some(0))
    bfm.write(0x41003000L, 0xffffffffL)
    bfm.readExpect(0x41003000L, Some(0x3)) // only en/loopback bits are writable
    u(dut.csr("uart0.ctrl.en")).expect(1.U)
    u(dut.csr("uart0.ctrl.loopback")).expect(1.U)
  }

  it should "pulse the rotrg trigger only in the access phase of a read" in withDut { (dut, bfm) =>
    fork {
      u(dut.csr("uart0.data.rxData.data")).poke(0xaa.U)
      bfm.readExpect(0x41003008L, Some(0xaa))
    }.fork
      .withRegion(Monitor) {
        dut.clock.step(1) // wait for access phase
        u(dut.csr("uart0.data.rxData.trg")).expect(true.B)
      }.joinAndStep()
    u(dut.csr("uart0.data.rxData.trg")).expect(false.B)
  }

  it should "pulse the wotrg trigger and hold the written value" in withDut { (dut, bfm) =>
    fork {
      bfm.write(0x41003008L, 0x5aL)
    }.fork
      .withRegion(Monitor) {
        dut.clock.step(1) // wait for access phase
        u(dut.csr("uart0.data.txData.trg")).expect(true.B)
      }.joinAndStep()
    u(dut.csr("uart0.data.txData.trg")).expect(false.B)
    dut.apb.pwdata.poke(0x11.U) // bus now carries other data
    dut.clock.step(2)
    u(dut.csr("uart0.data.txData.data")).expect(0x5a.U)
    bfm.write(0x41004004L, 0x77L) // write to another register
    u(dut.csr("uart0.data.txData.data")).expect(0x5a.U)
  }

  it should "signal pslverr for invalid accesses" in withDut { (dut, bfm) =>
    bfm.readExpect(0x50000000L, None) // unmapped read
    assert(bfm.write(0x50000000L, 1).isEmpty) // unmapped write
    assert(bfm.write(0x80000000L, 1).isEmpty) // write to const
    assert(bfm.write(0x41004008L, 1).isEmpty) // write to ro
    assert(bfm.write(0x4100300cL, 1).isEmpty) // past the last uart register
    assert(bfm.write(0x41003000L, 1).isDefined) // valid write
  }

  it should "keep wr and rd access state consistent across back-to-back transfers" in withDut { (dut, _) =>
    val apb = dut.apb
    // Raw APB master: psel stays high between transfers, the next setup
    // phase directly follows the access phase of the previous transfer.
    def xfer(addr: Long, write: Boolean, data: Long = 0): (BigInt, Boolean) = {
      apb.psel.poke(true.B)
      apb.penable.poke(false.B)
      apb.paddr.poke(addr.U)
      apb.pwrite.poke(write.B)
      apb.pwdata.poke(data.U)
      dut.clock.step()
      apb.penable.poke(true.B)
      var waited = 0
      while (!apb.pready.peekBoolean()) { dut.clock.step(); waited += 1 }
      assert(waited == 0, "adapter must complete in the first access cycle")
      val res = (apb.prdata.peekInt(), apb.pslverr.peekBoolean())
      dut.clock.step()
      res
    }
    val r1 = xfer(0x41004004L, write = true, data = 0x1234L) // gpio0.dir
    val r2 = xfer(0x41004004L, write = false)
    val r3 = xfer(0x41004014L, write = true, data = 0xabcdL) // gpio1.dir
    val r4 = xfer(0x41004014L, write = false)
    val r5 = xfer(0x41004004L, write = false)
    val r6 = xfer(0x50000000L, write = false) // error in the middle of a stream
    val r7 = xfer(0x80000000L, write = false)
    apb.psel.poke(false.B)
    assert(!r1._2)
    assert(r2 == (BigInt(0x1234), false))
    assert(!r3._2)
    assert(r4 == (BigInt(0xabcd), false))
    assert(r5 == (BigInt(0x1234), false)) // gpio0 unaffected by gpio1 write
    assert(r6._2)
    assert(r7 == (BigInt(0xdeadbeefL), false))
  }

  "Python CSR Generator" should "generate CSR adapter for soc.xlsx" in {

    test(new PythonSocAdapterWrapper)
      .withAnnotations(Seq(VerilatorBackendAnnotation) ++ annotations) {
        dut =>
          // instantiate APB master Bus Functional Model (BFM)
          val bfm = new ApbMasterBfm(
            dut.clock,
            dut.reset,
            dut.io.psel,
            dut.io.penable,
            dut.io.paddr,
            dut.io.pwrite,
            dut.io.pwdata,
            dut.io.prdata,
            dut.io.pready,
            dut.io.pslverr
          )

          // reset DUT
          bfm.reset()

          // test read-only (ro) field
          dut.io.gpio0_dataIn.poke(0xdeadbeefL.U)
          bfm.readExpect(0x41004008, Some(0xdeadbeefL)) // gpio0 dataIn

          // test read trigger
          // spawn two threads: one to perform the apb read, another to monitor the trigger signal
          fork {
            dut.io.uart0_data_rxData.poke(0xaa.U) // uart0 rxData
            bfm.readExpect(0x41003008, Some(0xaa))
          }.fork
            .withRegion(Monitor) { // use thread ordering
              // this thread runs in the monitor region, so it can observe signals *after* the other thread has poked them
              dut.clock.step(1) // wait for access phase
              dut.io.uart0_data_rxData_trg.expect(1.B)
          }.joinAndStep()

          // test write trigger
          // spawn two threads: one to perform the apb write, another to monitor the trigger signal
          fork {
            bfm.write(0x41003008, 0x5aL) // uart0 txData
            dut.io.uart0_data_txData.expect(0x5a.U)
          }.fork
            .withRegion(Monitor) { // use thread ordering
              // this thread runs in the monitor region, so it can observe signals *after* the other thread has poked them
              dut.clock.step(1) // wait for access phase
              dut.io.uart0_data_txData_trg.expect(1.B)
          }.joinAndStep()

          // test internal register read/write
          bfm.write(0x41003000, 0xdeadbeefL)
          bfm.readExpect(0x41003000, Some(0x3)) // only lower 2 bits are writable

          // test constant
          bfm.readExpect(0x80000000L, Some(0xdeadbeefL))

          // test invalid address
          bfm.readExpect(0x50000000L, None)

      }

  }

}
