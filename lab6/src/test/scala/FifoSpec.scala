/*
 * Every test listing from the "Testing and Continuous Integration" slides,
 * in slide order. The comments name the slide each block comes from.
 */

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
// Slide "Getting Started": imports and the class header
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatestplus.scalacheck._

class FifoSpec extends AnyFlatSpec
    with ChiselScalatestTester
    with ScalaCheckPropertyChecks {

  // Slide "Test-Driven Development (TDD)"
  "A stage" should "start empty" in {
    test(new FifoStage(8)) { c =>
      c.io.empty.expect(true.B)
      c.io.full.expect(false.B)
    }
  }

  // Slide "Property-Based Testing in Chisel"
  "A stage" should "pop what was pushed" in {
    forAll(Gen.choose(0, 255)) { in =>
      test(new FifoStage(8)) { c =>
        c.io.push.poke(true.B)
        c.io.input.poke(in.U)
        c.clock.step()
        c.io.push.poke(false.B)
        c.io.pop.poke(true.B)
        c.io.output.expect(in.U)
        c.clock.step()
        c.io.empty.expect(true.B)
      }
    }
  }

  // Slide "Example: Order is Preserved"
  val depth = 8
  val items = Gen.resize(depth, Gen.listOf(Gen.choose(0, 255)))
  "A FIFO" should "pop items in push order" in {
    forAll(items) { list =>
      test(new Fifo(8, depth)) { c =>
        for (item <- list) {  // push
          while (c.io.full.peek().litToBoolean) c.clock.step()
          c.io.push.poke(true.B)
          c.io.input.poke(item.U)
          c.clock.step()
          c.io.push.poke(false.B)
        }
        for (item <- list) {  // pop
          while (c.io.empty.peek().litToBoolean) c.clock.step()
          c.io.output.expect(item.U)
          c.io.pop.poke(true.B)
          c.clock.step()
          c.io.pop.poke(false.B)
        }
      }
    }
  }

  // Slide "Generators, Arbitrary and Shrinking": a custom default generator ...
  implicit val bytes: Arbitrary[List[Int]] =
    Arbitrary(Gen.resize(8, Gen.listOf(Gen.choose(0, 255))))

  // ... and the tip for switching shrinking off for a type
  // Uncomment next line to turn off shrinking for Ints
  //implicit val noShrink: Shrink[Int] = Shrink.shrinkAny

  // forAll without an explicit generator picks up the implicit Arbitrary above
  "A FIFO" should "pop items in push order, using the implicit Arbitrary" in {
    forAll { (list: List[Int]) =>
      test(new Fifo(8, depth)) { c =>
        for (item <- list) {
          while (c.io.full.peek().litToBoolean) c.clock.step()
          c.io.push.poke(true.B)
          c.io.input.poke(item.U)
          c.clock.step()
          c.io.push.poke(false.B)
        }
        for (item <- list) {
          while (c.io.empty.peek().litToBoolean) c.clock.step()
          c.io.output.expect(item.U)
          c.io.pop.poke(true.B)
          c.clock.step()
          c.io.pop.poke(false.B)
        }
      }
    }
  }

  // Slide "Getting Started": more runs with minSuccessful
  "A stage" should "pop what was pushed, 200 times" in {
    forAll(Gen.choose(0, 255), minSuccessful(200)) { in =>
      test(new FifoStage(8)) { c =>
        c.io.push.poke(true.B)
        c.io.input.poke(in.U)
        c.clock.step()
        c.io.push.poke(false.B)
        c.io.pop.poke(true.B)
        c.io.output.expect(in.U)
        c.clock.step()
        c.io.empty.expect(true.B)
      }
    }
  }
}
