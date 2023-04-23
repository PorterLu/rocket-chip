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

object MTIMERConsts
{
    def mtimecmpOffset(hart: Int) = hart * mtimecmpBytes
    def mtimecmpBytes = 8
    def mtimeWidth = 64
    def mtimecmpSize = 0x8000
    def mtimeSize = 0x100
    def ints = 1 
}

case class MTIMERParams(MTIMEBaseAddress: BigInt = 0x02010000, MTIMECMPBaseAddress: BigInt = 0x02008000, intStages: Int = 0)
{
    def mtimeAddress = AddressSet(MTIMEBaseAddress, MTIMERConsts.mtimeSize - 1)
    def mtimecmpAddress = AddressSet(MTIMECMPBaseAddress, MTIMERConsts.mtimecmpSize - 1)
}

case object MTIMERKey extends Field[Option[MTIMERParams]](None)

case class MTIMERAttachParams(
    slaveWhere: TLBusWrapperLocation = CBUS
)

case object MTIMERAttachKey extends Field(MTIMERAttachParams())

class MTIMER(params: MTIMERParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
    import MTIMERConsts._ 

    val device = new SimpleDevice("mtimer", Seq("riscv,mtimer")) {
        override val alwaysExtended = true
    }

    val mtimeNode: TLRegisterNode = TLRegisterNode(
        address     = Seq(params.mtimeAddress),
        device      = device,
        beatBytes   = beatBytes
    )

    val mtimecmpNode: TLRegisterNode = TLRegisterNode(
        address     = Seq(params.mtimecmpAddress),
        device      = device,
        beatBytes   = beatBytes
    )

    val intnode: IntNexusNode = IntNexusNode(
        sourceFn    = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int")))))},
        sinkFn      = { _ => IntSinkPortParameters(Seq(IntSinkParameters()))},
        outputRequiresInput = false
    )

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        Annotated.params(this, params)
        require (intnode.edges.in.size == 0, "MTIMER Device only produces interrupts; it does not accept them")

        val io = IO(new Bundle{
            val rtcTick = Input(Bool())
        })

        val time = RegInit(0.U(mtimeWidth.W))
        when (io.rtcTick) { time := time + 1.U }

        val nTiles = intnode.out.size
        val timecmp = Seq.fill(nTiles) { Reg(UInt(mtimeWidth.W)) }
        
        val (intnode_out, _) = intnode.out.unzip
        intnode_out.zipWithIndex.foreach { case (int, i) =>
            int(0) := ShiftRegister(time.asUInt >= timecmp(i).asUInt, params.intStages)
        }

        /*
         * 0 mtimecmp hart 0 lo
         * 4 mtimecmp hart 0 hi
         * 8 mtimecmp hart 1 lo
         * c mtimecmp hart 1 hi
         */
         mtimecmpNode.regmap(
            0 -> timecmp.zipWithIndex.flatMap{ case (t, i) => RegFieldGroup(s"mtimecmp_$i", Some(s"MTIMECMP for hart $i"),
                RegField.bytes(t, Some(RegFieldDesc(s"mtimecmp_$i", "", reset=None))))},
         )

         mtimeNode.regmap(
            0 -> RegFieldGroup("mtime", Some("Timer Register"),
                RegField.bytes(time, Some(RegFieldDesc("mtime", "", reset=Some(0), volatile=true))))
         )
    }
}

trait CanHavePeripheryMTIMER { this: BaseSubsystem => 
    val mtimerOpt = p(MTIMERKey).map { params =>
        val tlbus = locateTLBusWrapper(p(MTIMERAttachKey).slaveWhere)
        val mtimer = LazyModule(new MTIMER(params, cbus.beatBytes))
        mtimer.mtimeNode := tlbus.coupleTo("mtime") { TLFragmenter(tlbus) := _ }
        mtimer.mtimecmpNode := tlbus.coupleTo("mtimecmp") { TLFragmenter(tlbus) := _ } 

        InModuleBody {
            mtimer.module.clock := tlbus.module.clock
            mtimer.module.reset := tlbus.module.reset
        }

        mtimer
    }
}