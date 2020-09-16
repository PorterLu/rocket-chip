// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._

/** Collects interrupts from internal and external devices and feeds them into the PLIC */ 
class InterruptBusWrapper(implicit p: Parameters) extends SimpleLazyModule with LazyScope with HasClockDomainCrossing {
  override def shouldBeInlined = true
  val int_bus = LazyModule(new IntXbar)
  private val int_in_xing = this.crossIn(int_bus.intnode)
  private val int_out_xing = this.crossOut(int_bus.intnode)
  def from(xing: ClockCrossingType) = int_in_xing(xing)
  def to(xing: ClockCrossingType) = int_out_xing(xing)
  def fromAsync: IntInwardNode = from(AsynchronousCrossing(8,3))
  def fromRational: IntInwardNode = from(RationalCrossing())
  def fromSync: IntInwardNode = from(SynchronousCrossing())
  def toPLIC: IntOutwardNode = to(NoCrossing)
}

/** Specifies the number of external interrupts */
case object NExtTopInterrupts extends Field[Int](0)

/** This trait adds externally driven interrupts to the system. 
  * However, it should not be used directly; instead one of the below
  * synchronization wiring child traits should be used.
  */
abstract trait HasExtInterrupts { this: BaseSubsystem =>
  private val device = new Device with DeviceInterrupts {
    def describe(resources: ResourceBindings): Description = {
      Description("soc/external-interrupts", describeInterrupts(resources))
    }
  }

  val nExtInterrupts = p(NExtTopInterrupts)
  val extInterrupts = IntSourceNode(IntSourcePortSimple(num = nExtInterrupts, resources = device.int))
}

/** This trait should be used if the External Interrupts have NOT
  * already been synchronized to the Periphery (PLIC) Clock.
  */
trait HasAsyncExtInterrupts extends HasExtInterrupts { this: BaseSubsystem =>
  if (nExtInterrupts > 0) {
    ibus.fromAsync := extInterrupts
  }
}

/** This trait can be used if the External Interrupts have already been synchronized
  * to the Periphery (PLIC) Clock.
  */
trait HasSyncExtInterrupts extends HasExtInterrupts { this: BaseSubsystem =>
  if (nExtInterrupts > 0) {
    ibus.fromSync := extInterrupts
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasExtInterruptsBundle {
  val interrupts: UInt

  def tieOffInterrupts(dummy: Int = 1) {
    interrupts := UInt(0)
  }
}

/** This trait performs the translation from a UInt IO into Diplomatic Interrupts.
  * The wiring must be done in the concrete LazyModuleImp. 
  */
trait HasExtInterruptsModuleImp extends LazyModuleImp with HasExtInterruptsBundle {
  val outer: HasExtInterrupts
  val interrupts = IO(UInt(INPUT, width = outer.nExtInterrupts))

  outer.extInterrupts.out.map(_._1).flatten.zipWithIndex.foreach { case(o, i) => o := interrupts(i) }
}
