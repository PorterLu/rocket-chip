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

case MSWIParams(baseAdrress: BigInt = 0x02000000, intStages: Int = 0)
{
    def address = AddressSet(baseAddress, MSWIConsts.size - 1)
}

case object MSWIKey extends Field[Option[MSWIParams]](None)

case class MSWIAttachParams(
    slaveWhere: TLBusWrapperLocation = CBUS
)

case class MSWIAttachKey extends Field(MSWIAttachParams())

class MSWI(params: MSWIParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
    import MSWIConsts._ 

    val device = new SimpleDevice("mswi", Seq("riscv,mswi")) {
        override val alwaysExtended = true
    }

    val node: TLRegisterNode = TLRegisterNode(
        address     = Seq(params.address)
        device      = device
        beatBytes   = beatBytes
    )

    val intnode : IntNexusNode = IntNexusNode(
        sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
        sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
        outputRequiresInput = false
    )

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        Annotated.params(this, params)
        require (intnode.edges.in.size == 0, "MSWI only produces interrupts; it does not accept them")
    }

    val nTiles = intnode.out.size
    val ipi    = Seq.fill(nTiles) { RegInit(0.U(1.W)) }

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
        int(0) := ShiftRegister(ipi(i)(0), params.intStages)
    }

    node.regmap(
        0 -> RegFieldGroup ("msip", Some("MSIP Bits"), ipi.zipWithIndex.flatMap{ case (r, i) =>
        RegField(1, r, RegFieldDesc(s"msip_$i", s"MSIP bit for Hart $i", reset=Some(0))) :: RegField(ipiWidth - 1) :: Nil }),
    )
}

trait CanHavePeripheryMSWI { this: BaseSubsystem => 
    val mswiOpt = p(MSWIKey).map { params =>
        val tlbus = locateTLBusWrapper(p(MSWIAttachKey).slaveWhere)
        val mswi = LazyModule(new MSWI(params, cbus.beatBytes))
        mswi.node := tlbus.coupleTo("mswi") { TLFragmenter(tlbus) := _ }

        InModuleBody {
            mswi.module.clock := tlbus.module.clock
            mswi.module.reset := tlbus.module.reset
        }

        mswi
    }
}
