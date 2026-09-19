
import help._

import chisel3._
import chisel3.util._

class ApbPort extends Bundle {
  val psel = Input(Bool())
  val penable = Input(Bool())
  val pwrite = Input(Bool())
  val paddr = Input(UInt(32.W))
  val pwdata = Input(UInt(32.W))
  val prdata = Output(UInt(32.W))
  val pready = Output(Bool())
  val pslverr = Output(Bool())
}

class CsrAdapter(descriptionSheetPath: String) extends Module {

  val sheets = Sheet.load(descriptionSheetPath)
  val map = sheets("Map")
  println(map)

  val apb = IO(new ApbPort)
  
  val csr = IO(new DynamicBundle(
    Seq(sheets(map.column("Block").head).column("Register").head -> Output(UInt(32.W)))
  ))


  apb := DontCare
  apb.pready := 1.B
  apb.pslverr := 1.B
  csr := DontCare

}

object CsrAdapter extends App {
  emitVerilog(
    new CsrAdapter("soc.xlsx"),
    Array("--target-dir", "generated")
  )
}