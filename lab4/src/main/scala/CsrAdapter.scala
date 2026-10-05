import help._

import chisel3._
import chisel3.util._
import scala.collection.mutable

object FieldType extends Enumeration {
  val Rw = Value("rw")
  val Ro = Value("ro")
  val WoTrg = Value("wotrg")
  val RoTrg = Value("rotrg")
  val Const = Value("const")
}

case class Field(
    name: String,
    typ: FieldType.Value,
    range: (Int, Int),
    init: String
) {
  def width: Int = range._1 - range._2 + 1
}

case class IpBlock(name: String, base_address: UInt)
case class Register(name: String, offset: UInt, fields: Seq[Field])

case class CsrRegister(typ: IpBlock, reg: Register) {
  def address: UInt = typ.base_address + reg.offset
}

//Pre: String starts with "0x" and is hexadecimal
object HardwareParser {
  def encodeHex(s: String, width: Int = 32): UInt = {
    val t = s.trim
    if (t.startsWith("0x") || t.startsWith("0X"))
      ("h" + t.drop(2)).U(width.W)
    else
      t.toDouble.toLong.U(width.W)
  }

  def encodeRange(s: String): (Int, Int) = {
    val range = s.trim().split(":").map(_.trim().toInt)
    (range(0), range(1))
  }
}

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
  val REG_NAME_IND = 0
  val REG_OFF_IND = 1
  val REG_FLD_IND = 2
  val REG_TYP_IND = 3
  val REG_RANG_IND = 4
  val REG_INIT_IND = 5

  val sheets = Sheet.load(descriptionSheetPath)
  val map = sheets("Map")
  println(map)

  val apb = IO(new ApbPort)

  def createKeyName(r: CsrRegister, f: Field): String = {
    if (f.name.isEmpty()) s"${r.typ.name}.${r.reg.name}"
    else s"${r.typ.name}.${r.reg.name}.${f.name}"
  }

  def mapRegisterAction(
      reg: CsrRegister,
      field: Field
  ): Seq[(String, Data)] = {
    val name_created = createKeyName(reg, field)
    field.typ match {
      case FieldType.Rw    => Seq(name_created -> Output(UInt(field.width.W)))
      case FieldType.Ro    => Seq(name_created -> Input(UInt(field.width.W)))
      case FieldType.WoTrg =>
        Seq(
          s"${name_created}.data" -> Output(UInt(field.width.W)),
          s"${name_created}.trg" -> Output(Bool())
        )
      case FieldType.RoTrg =>
        Seq(
          s"${name_created}.data" -> Input(UInt(field.width.W)),
          s"${name_created}.trg" -> Output(Bool())
        )
      case FieldType.Const => Seq()
    }
  }

  def get_all_regs(): Seq[CsrRegister] = {
    val blocks = map.column("Block")
    val names = map.column("Name")
    val bases = map.column("Base Address")
    for {
      i <- blocks.indices
      block_sheet = sheets(blocks(i))
      r_name <- block_sheet.column("Register").distinct
    } yield {
      val rows = block_sheet.filterRows(r => r(REG_NAME_IND) == r_name)
      val fields = rows.map(r =>
        Field(
          r(REG_FLD_IND).trim,
          FieldType.withName(r(REG_TYP_IND).trim.toLowerCase),
          HardwareParser.encodeRange(r(REG_RANG_IND)),
          r(REG_INIT_IND)
        )
      )
      val offset = HardwareParser.encodeHex(rows.head(REG_OFF_IND))
      val ipBlock = IpBlock(names(i), HardwareParser.encodeHex(bases(i)))

      CsrRegister(ipBlock, Register(r_name, offset, fields))
    }
  }

  def writeOp(reg: CsrRegister, field: Field, hit: Bool): Unit = {
    val name = createKeyName(reg, field)
    val data = apb.pwdata(field.range._1, field.range._2)

    field.typ match {
      case FieldType.Rw =>
        when(wr_access && hit) {
          flops(name) := data
        }
      case FieldType.WoTrg => {
        csr(s"${name}.data") := data
        csr(s"${name}.trg") := wr_access && hit
      }
      case _ =>
    }
  }

  def readOp(reg: CsrRegister, field: Field, hit: Bool): Option[UInt] = {
    val name = createKeyName(reg, field)

    field.typ match {
      case FieldType.Rw    => Some(flops(name))
      case FieldType.Ro    => Some(csr(name).asUInt)
      case FieldType.RoTrg => {
        csr(s"${name}.trg") := rd_access && hit
        Some(csr(s"${name}.data").asUInt)
      }
      case FieldType.Const =>
        Some(HardwareParser.encodeHex(field.init, field.width))
      case FieldType.WoTrg => None
    }
  }

  val csr_registers = get_all_regs()
  val ports: Seq[(String, Data)] =
    csr_registers.flatMap(cr =>
      cr.reg.fields.flatMap(f => mapRegisterAction(cr, f))
    )

  val csr = IO(new DynamicBundle(ports))

  val wr_access = RegInit(false.B)
  val rd_access = RegInit(false.B)
  wr_access := Mux(wr_access, false.B, apb.psel && apb.pwrite)
  rd_access := Mux(rd_access, false.B, apb.psel && !apb.pwrite)
  apb.pready := wr_access || rd_access

  val flops = mutable.Map[String, UInt]()
  for (cr <- csr_registers; f <- cr.reg.fields if f.typ == FieldType.Rw) {
    val name = createKeyName(cr, f)
    val flop = RegInit(HardwareParser.encodeHex(f.init, f.width))
    flops(name) = flop
    csr(name) := flop
  }
  apb.prdata := 0.U
  val readHits = mutable.ArrayBuffer[Bool]()
  val writeHits = mutable.ArrayBuffer[Bool]()

  csr_registers.foreach { csr_register =>
    val hit = apb.paddr === csr_register.address
    val fields = csr_register.reg.fields

    fields.foreach(f => writeOp(csr_register, f, hit))

    val parts = fields.flatMap(f =>
      readOp(csr_register, f, hit).map(v => v << f.range._2)
    )
    if (parts.nonEmpty) {
      // puts all shifted values into one word
      when(hit) { apb.prdata := parts.reduce(_ | _) }
      readHits += hit
    }
    if (fields.exists(f => f.typ == FieldType.Rw || f.typ == FieldType.WoTrg))
      writeHits += hit
  }

  apb.pslverr := false.B
  when(rd_access) { apb.pslverr := !readHits.foldLeft(false.B)(_ || _) }
  when(wr_access) { apb.pslverr := !writeHits.foldLeft(false.B)(_ || _) }
}

object CsrAdapter extends App {
  emitVerilog(
    new CsrAdapter("soc.xlsx"),
    Array("--target-dir", "generated")
  )
}
