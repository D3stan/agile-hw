import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MaxFinderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  "MaxFinder" should "find the maximum value in a Vec" in {
    test(new MaxFinder(4, 8)) { dut =>
      Seq(
        Seq(1, 5, 3, 2),
        Seq(8, 2, 7, 1),
        Seq(4, 4, 4, 4),
        Seq(0, 9, 3, 6),
        Seq(0, 0, 0, 0),
        Seq(1, 1, 1, 1)
      ).foreach { inputs =>
      
        inputs.zipWithIndex.foreach { case (value, i) =>
          dut.io.in(i).poke(value.U)
        }
      
        dut.clock.step()
      
        dut.io.max.expect(inputs.max.U)
      }
    }
  }
}
