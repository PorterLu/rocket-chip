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

object MSWIConsts
{
  def msipOffset(hart: Int) = hart * msipBytes
  def msipBytes = 4
  def size = 0x4000
  def ipiWidth = 32
  def ints = 1
}

case class MSWIParams(BaseAddress: BigInt = 0x02000000, intStages: Int = 0)
{
  def address = AddressSet(BaseAddress, MSWIConsts.size - 1)
}

case class MSWIAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object MSWIAttachKey extends Field(MSWIAttachParams())

class MSWI(mswiParams: MSWIParams, mtimerParams: MTIMERParams, isACLINT: Boolean = false, beatBytes: Int)(implicit p: Parameters) extends LazyModule 
{
  import MSWIConsts._ 

  val device = if (isACLINT) { 
      new SimpleDevice("mswi", Seq("riscv,aclint-mswi")) {
        override val alwaysExtended = true
      }
    } else {
      new SimpleDevice("clint", Seq("riscv,clint0")) {
        override val alwaysExtended = true
      }
    }

  val node: TLRegisterNode = if (isACLINT) {
    TLRegisterNode(
      address     = Seq(mswiParams.address),
      device      = device,
      beatBytes   = beatBytes
    )
  } else {
    TLRegisterNode(
      address     = Seq(AddressSet(mswiParams.address.base, 0x10000 - 1)),
      device      = device,
      beatBytes   = beatBytes
    )
  }

  val ints = if (isACLINT) {
    MSWIConsts.ints
  } else {
    MSWIConsts.ints + MTIMERConsts.ints
  }

  val intnode : IntNexusNode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    if (isACLINT) {
      Annotated.params(this, mswiParams)
    } else {
      Annotated.params(this, mswiParams)
      Annotated.params(this, mtimerParams)
    }

    require (intnode.edges.in.size == 0, "MSWI only produces interrupts; it does not accept them")

    val nTiles  = intnode.out.size
    val ipi     = Seq.fill(nTiles) { RegInit(0.U(1.W)) }
    
    val io = IO(new Bundle {
        val rtcTick = Input(Bool())
    })
    
    val timecmp = if (!isACLINT) { Seq.fill(nTiles) { Reg(UInt(MTIMERConsts.mtimeWidth.W))} } else { Seq.fill(nTiles){ Reg(UInt(0.W))} }
    val time    = if (!isACLINT) { RegInit(0.U(MTIMERConsts.mtimeWidth.W)) } else { RegInit(0.U(0.W)) }

    if (!isACLINT)
    {
      when (io.rtcTick) { time := time + 1.U }
    }

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := ShiftRegister(ipi(i)(0), mswiParams.intStages)
      if (!isACLINT) {
        int(1) := ShiftRegister(time.asUInt >= timecmp(i).asUInt, mtimerParams.intStages)
      }
    }

    /* aclint:
     * 0 msip hart 0
     * 4 msip hart 1
     * 
     * clint:
     * 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * 400c mtimecmp hart 1 hi
     * bff8 mtime lo
     * bffc mtime hi
     */
    if (!isACLINT) {
      node.regmap(
        0 -> RegFieldGroup ("msip", Some("MSIP Bits"), ipi.zipWithIndex.flatMap{ case (r, i) =>
          RegField(1, r, RegFieldDesc(s"msip_$i", s"MSIP bit for Hart $i", reset=Some(0))) :: RegField(MSWIConsts.ipiWidth - 1) :: Nil }),
        0x4000 + MTIMERConsts.mtimecmpOffset(0) -> timecmp.zipWithIndex.flatMap{ case (t, i) => RegFieldGroup(s"mtimecmp_$i", Some(s"MTIMECMP for hart $i"),
          RegField.bytes(t, Some(RegFieldDesc(s"mtimecmp_$i", "", reset=None))))},
        0xbff8 -> RegFieldGroup("mtime", Some("Timer Register"),
          RegField.bytes(time, Some(RegFieldDesc("mtime", "", reset=Some(0), volatile=true))))
      )
    } else {
      node.regmap(
        0 -> RegFieldGroup ("msip", Some("MSIP Bits"), ipi.zipWithIndex.flatMap{ case (r, i) =>
          RegField(1, r, RegFieldDesc(s"msip_$i", s"MSIP bit for Hart $i", reset=Some(0))) :: RegField(ipiWidth - 1) :: Nil })
      )
    }
  }
}

