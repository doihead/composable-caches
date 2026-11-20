/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MetaData._
import chisel3.experimental.dataview._
import freechips.rocketchip.util.DescribedSRAM

class DirectoryEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val dirty   = Bool() // true => TRUNK or TIP
  val state   = UInt(params.stateBits.W)
  val clients = UInt(params.clientBits.W)
  val tag     = UInt(params.tagBits.W)
  val spad_trunk = Option.when(params.cache.hasSpad)(Bool())
}

class DirectoryWrite(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set  = UInt(params.setBits.W)
  val way  = UInt(params.wayBits.W)
  val is_spad = Bool()
  val data = new DirectoryEntry(params)
}

class DirectoryRead(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val tag = UInt(params.tagBits.W)
  val is_spad = Bool()
  val spad_way = UInt(params.wayBits.W)
}

class DirectoryResult(params: InclusiveCacheParameters) extends DirectoryEntry(params)
{
  val hit = Bool()
  val way = UInt(params.wayBits.W)
  val is_spad = Bool()
}

class Directory(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val write  = Flipped(Decoupled(new DirectoryWrite(params)))
    val read   = Flipped(Valid(new DirectoryRead(params))) // sees same-cycle write
    val result = Valid(new DirectoryResult(params))
    val ready  = Bool() // reset complete; can enable access
    val enable_ways_req = Flipped(Valid(UInt(log2Ceil(params.cache.ways + 1).W)))
  })

  val enabled_ways = RegEnable(io.enable_ways_req.bits,
    params.cache.initialWaysEnabled.U(log2Ceil(params.cache.ways + 1).W),
    io.enable_ways_req.valid)
  val enabled_ways_mask = ((1.U(params.cache.ways.W) << enabled_ways) - 1.U)(params.cache.ways - 1, 0)

  val codeBits = new DirectoryEntry(params).getWidth

  val cc_dir =  DescribedSRAM(
    name = "cc_dir",
    desc = "Directory RAM",
    size = params.cache.sets,
    data = Vec(params.cache.ways, UInt(codeBits.W))
  )

  val write = Queue(io.write, 1) // must inspect contents => max size 1
  // a flow Q creates a WaR hazard... this MIGHT not cause a problem
  // a pipe Q causes combinational loop through the scheduler

  // Wiping the Directory with 0s on reset has ultimate priority
  val wipeCount = RegInit(0.U((params.setBits + 1).W))
  val wipeOff = RegNext(false.B, true.B) // don't wipe tags during reset
  val wipeDone = wipeCount(params.setBits)
  val wipeSet = wipeCount(params.setBits - 1,0)

  io.ready := wipeDone
  when (!wipeDone && !wipeOff) { wipeCount := wipeCount + 1.U }
  assert (wipeDone || !io.read.valid)

  // Be explicit for dumb 1-port inference
  val ren = io.read.valid
  val wen = (!wipeDone && !wipeOff) || write.valid
  assert (!io.read.valid || wipeDone)

  require (codeBits <= 256)

  write.ready := !io.read.valid
  val writeBits = WireInit(write.bits)
  if (params.cache.hasSpad) {
    when (write.bits.is_spad) {
      writeBits.data.spad_trunk.get := write.bits.data.state === TRUNK
      writeBits.data.state := INVALID
    }
  }
  when (!ren && wen) {
    cc_dir.write(
      Mux(wipeDone, writeBits.set, wipeSet),
      VecInit.fill(params.cache.ways) { Mux(wipeDone, writeBits.data.asUInt, 0.U) },
      UIntToOH(writeBits.way, params.cache.ways).asBools.map(_ || !wipeDone))
  }

  val ren1 = RegInit(false.B)
  val ren2 = if (params.micro.dirReg) RegInit(false.B) else ren1
  ren2 := ren1
  ren1 := ren

  val bypass_valid = params.dirReg(write.valid)
  val bypass = params.dirReg(writeBits, ren1 && write.valid)
  val regout = params.dirReg(cc_dir.read(io.read.bits.set, ren), ren1)
  val tag = params.dirReg(RegEnable(io.read.bits.tag, ren), ren1)
  val set = params.dirReg(RegEnable(io.read.bits.set, ren), ren1)
  val is_spad = params.dirReg(RegEnable(io.read.bits.is_spad, ren), ren1)
  val spad_way = params.dirReg(RegEnable((params.cache.ways - 1).U - io.read.bits.spad_way, ren), ren1)

  // Compute the victim way in case of an evicition
  val victimLFSR = random.LFSR(width = 16, params.dirReg(ren))(InclusiveCacheParameters.lfsrBits-1, 0)
  val victimSums = if (params.cache.hasSpad) {
    VecInit((1 to params.cache.ways).map { nEnabled =>
      VecInit(Seq.tabulate(params.cache.ways) { i =>
        ((BigInt(1) << InclusiveCacheParameters.lfsrBits)*i / nEnabled)
          .min(BigInt(1) << InclusiveCacheParameters.lfsrBits)
          .U((InclusiveCacheParameters.lfsrBits + 1).W)
      })
    })(enabled_ways - 1.U)
  } else {
    VecInit(Seq.tabulate(params.cache.ways) { i =>
      ((BigInt(1) << InclusiveCacheParameters.lfsrBits)*i / params.cache.ways)
        .U((InclusiveCacheParameters.lfsrBits + 1).W)
    })
  }
  val victimLTE  = Cat(victimSums.zipWithIndex.map { case (threshold, i) => threshold <= victimLFSR && enabled_ways_mask(i.U) }.reverse)
  val victimSimp = Cat(0.U(1.W), victimLTE(params.cache.ways-1, 1), 1.U(1.W))
  val victimWayOH = victimSimp(params.cache.ways-1,0) & ~(victimSimp >> 1)
  val victimWay = OHToUInt(victimWayOH)
  assert (!ren2 || victimLTE(0) === 1.U)
  assert (!ren2 || ((victimSimp >> 1) & ~victimSimp) === 0.U) // monotone
  assert (!ren2 || PopCount(victimWayOH) === 1.U)

  val setQuash = bypass_valid && bypass.set === set
  val tagMatch = bypass.data.tag === tag
  val wayMatch = bypass.way === victimWay
  val spadWayMatch = bypass.way === spad_way
  val spadQuash = setQuash && bypass.is_spad && spadWayMatch

  val ways = regout.map(d => d.asTypeOf(new DirectoryEntry(params)))
  
  val hits_cache = Cat(ways.zipWithIndex.map { case (w, i) =>
    w.tag === tag && w.state =/= INVALID && (!setQuash || i.U =/= bypass.way) && enabled_ways_mask(i)
  }.reverse)
  val hits_spad = UIntToOH(spad_way)

  val hits = Mux(is_spad, hits_spad, hits_cache)

  val hit = hits.orR

  io.result.valid := ren2
  val dirent = Mux(is_spad && spadQuash, bypass.data, Mux(hit, Mux1H(hits, ways), Mux(setQuash && (tagMatch || wayMatch || is_spad), bypass.data, Mux1H(victimWayOH, ways))))
  io.result.bits.viewAsSupertype(chiselTypeOf(bypass.data)) := dirent
  when (is_spad) {
    io.result.bits.state := (if (params.cache.hasSpad) Mux(dirent.spad_trunk.get, TRUNK, TIP) else INVALID)
    io.result.bits.hit := !enabled_ways_mask(spad_way)
    io.result.bits.way := spad_way
  } .otherwise {
    io.result.bits.hit := hit || (setQuash && tagMatch && bypass.data.state =/= INVALID)
    io.result.bits.way := Mux(hit, OHToUInt(hits), Mux(setQuash && tagMatch, bypass.way, victimWay))
  }
  io.result.bits.is_spad := is_spad

  when (ren2) {
    when (is_spad) {
      assert(params.cache.hasSpad.B, "Scratchpad access in cache without scrachpad support")
      when (enabled_ways_mask(io.result.bits.way)) {
        assert(!io.result.bits.hit, "Invalid scratchpad read hit")
      } .otherwise {
        assert(io.result.bits.hit, "Valid scratchpad read missed")
      }
    } .otherwise {
      assert(enabled_ways_mask(io.result.bits.way), "Invalid selection of disabled way")
    }
  }

  params.ccover(ren2 && setQuash && tagMatch, "DIRECTORY_HIT_BYPASS", "Bypassing write to a directory hit")
  params.ccover(ren2 && setQuash && !tagMatch && wayMatch, "DIRECTORY_EVICT_BYPASS", "Bypassing a write to a directory eviction")

  def json: String = s"""{"clients":${params.clientBits},"mem":"${cc_dir.pathName}","clean":"${wipeDone.pathName}"}"""
}
