package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class SinkB(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val req = Decoupled(new FullRequest(params))
    val b = Flipped(Decoupled(new TLBundleB(params.inner.bundle)))
  })

  // No restrictions on the type of buffer
  val b = params.micro.innerBuf.b(io.b)

  b.ready := io.req.ready
  io.req.valid := b.valid

  val (tag, set, offset) = params.parseAddress(b.bits.address)

  io.req.bits.prio               := VecInit(2.U(3.W).asBools)
  io.req.bits.control.flush      := false.B
  io.req.bits.control.invalidate := false.B
  io.req.bits.opcode             := b.bits.opcode
  io.req.bits.param              := b.bits.param
  io.req.bits.size               := b.bits.size
  io.req.bits.source             := b.bits.source
  io.req.bits.offset             := offset
  io.req.bits.set                := set
  io.req.bits.tag                := tag
  io.req.bits.put                := 0.U
}
