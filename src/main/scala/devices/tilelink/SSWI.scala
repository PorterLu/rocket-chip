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

case class SSWIParams(baseAddress: BigInt = 0x02f00000, intStages: Int = 0)
{
  def address = AddressSet(baseAddress, SWIConsts.size - 1)
}

case class SSWIAttachParams(
  slaveWhere: TLBusWrapperLocation = CBUS
)

case object SSWIAttachKey extends Field(SSWIAttachParams())

class SSWI(sswiParams: SSWIParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule 
{
  import SWIConsts._ 

  val device = new SimpleDevice("sswi", Seq("riscv,aclint-sswi")) {
    override val alwaysExtended = true
  }

  val node: TLRegisterNode = TLRegisterNode(
    address   = Seq(sswiParams.address),
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
    Annotated.params(this, sswiParams)
    require(intnode.edges.in.size == 0, "SSWI Device only produces interrupts; it does not accept them")

    val sswiRegGroup: Seq[RegField] = SWI("s", intnode, sswiParams.intStages)

    val sswiRegMapping = Seq(
      0 -> sswiRegGroup
    ) 

    node.regmap(sswiRegMapping:_*)
  }

}