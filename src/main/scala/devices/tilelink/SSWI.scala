// See LICENSE.SiFive for license details.
  
package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util.ShiftRegister
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

object SSWIConsts {
  def ssipOffset(hart: Int) = hart * ssipBytes
  def ssipBytes = 4
  def size = 0x4000
  def ipiWidth = 32
  def ints = 1
}

case class SSWIParams(BaseAddress: BigInt = 0x02F00000, intStages: Int = 0)
{
  def address = AddressSet(BaseAddress, SSWIConsts.size - 1)
}

case class SSWIAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object SSWIAttachKey extends Field(SSWIAttachParams())

class SSWI(params: SSWIParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule 
{
  import SSWIConsts._ 

  val device = new SimpleDevice("sswi", Seq("riscv,aclint-sswi")) {
    override val alwaysExtended = true
  }

  val node: TLRegisterNode = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = beatBytes
  )

  val intnode: IntNexusNode = IntNexusNode(
    sourceFn    = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
    sinkFn      = { _ => IntSinkPortParameters(Seq(IntSinkParameters()))},
    outputRequiresInput = false
  )

  lazy val module = new Impl 
  class Impl extends LazyModuleImp(this) {
    Annotated.params(this, params)
    require(intnode.edges.in.size == 0, "SSWI Device only produces interrupts; it does not accespt them")

    val nTiles = intnode.out.size
    val ipi    = Seq.fill(nTiles) { RegInit(0.U(1.W)) }

    val io = IO(new Bundle {
      val rtcTick = Input(Bool())
    })

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(ipi(i)(0), params.intStages)
    }
    
    node.regmap(
      0 -> RegFieldGroup ("ssip", Some("SSIP Bits"), ipi.zipWithIndex.flatMap{ case (r, i) =>
        RegField(1, r, RegFieldDesc(s"ssip_$i", s"SSIP bit for Hart $i", reset=Some(0))) :: RegField(ipiWidth - 1) :: Nil })
    )
  }
}