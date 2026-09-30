/*
 * The FIFO running example from the "Testing and Continuous Integration" slides.
 *
 * A FifoStage holds one item. A Fifo is a chain of stages (a bubble FIFO):
 * an item ripples forward one stage per cycle, so the throughput is one item
 * every two cycles.
 *
 * Copyright: 2026, Technical University of Denmark, DTU Compute
 */

import chisel3._

/** The six ports used on the slides, shared by one stage and by the whole chain. */
class FifoIO(width: Int) extends Bundle {
  val push = Input(Bool())
  val input = Input(UInt(width.W))
  val full = Output(Bool())

  val pop = Input(Bool())
  val output = Output(UInt(width.W))
  val empty = Output(Bool())
}

/**
 * One stage of the FIFO. Behaviour, as on the unit-test slide:
 *  - starts empty
 *  - a push into an empty stage stores the value, which shows on output from the next cycle
 *  - a pop of a full stage empties it
 *  - a push into a full stage is ignored, unless it is popped in the same cycle
 *  - a pop of an empty stage does nothing (output stays at the last value)
 */
class FifoStage(width: Int) extends Module {
  val io = IO(new FifoIO(width))

  val data = RegInit(0.U(width.W))
  val valid = RegInit(false.B)

  when(io.pop && valid) {
    valid := false.B
  }
  when(io.push && (!valid || io.pop)) {
    data := io.input
    valid := true.B
  }

  io.output := data
  io.full := valid
  io.empty := !valid
}

/**
 * A FIFO of `depth` stages. Stage i hands its item to stage i+1 as soon as
 * stage i+1 is empty. Push and pop of the whole FIFO are the push of stage 0
 * and the pop of the last stage.
 */
class Fifo(width: Int, depth: Int) extends Module {
  require(depth >= 1, "a FIFO needs at least one stage")
  val io = IO(new FifoIO(width))

  val stages = Seq.fill(depth)(Module(new FifoStage(width)))

  for (i <- 0 until depth - 1) {
    val from = stages(i).io
    val to = stages(i + 1).io
    val transfer = from.full && to.empty
    to.push := transfer
    to.input := from.output
    from.pop := transfer
  }

  stages.head.io.push := io.push
  stages.head.io.input := io.input
  io.full := stages.head.io.full

  stages.last.io.pop := io.pop
  io.output := stages.last.io.output
  io.empty := stages.last.io.empty
}

/** Generate Verilog for an 8-bit, 4-stage FIFO with `sbt run`. */
object Fifo extends App {
  emitVerilog(new Fifo(8, 4))
}
