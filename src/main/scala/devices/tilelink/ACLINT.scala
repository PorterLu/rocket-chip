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

trait CanHavePeripheryACLINT { this: BaseSubsystem => 
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