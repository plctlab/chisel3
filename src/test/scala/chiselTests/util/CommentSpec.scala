// SPDX-License-Identifier: Apache-2.0

package chiselTests.util

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util.Comment.{AddAttr, AddDoc}
import firrtl.EmittedVerilogCircuitAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CommentSpec extends AnyFlatSpec with Matchers {
  behavior of "Comment"

  val moduleDoc = "This is a Module"
  val instanceDoc = "This is an Instance"
  val ioDoc = "This is an IO port"
  val regDoc = "This is a reg"
  val wireDoc = "This is a wire"
  val nodeDoc = "This is a Node"
  val attribute = "An attribute"

  class Reg(n: Int) extends Module {
    val io = IO(new Bundle() {
      val i = Input(UInt(n.W))
      val o = Output(UInt(n.W))
    })

    val reg = RegNext(io.i)
    io.o := reg
    reg.addDoc(regDoc)

    this.addDoc(moduleDoc)
  }

  class M(n: Int) extends Module {
    val io = IO(new Bundle() {
      val i = Input(UInt(n.W))
      val o = Output(UInt(n.W))
    })

    io.addDoc(ioDoc)

    this.addDoc(moduleDoc)

    val reg = Module(new Reg(n))
    reg.io.i := io.i
    reg.addDoc(instanceDoc) // This comment will be merged to this.addDoc in Reg class

    val wire = Wire(UInt(n.W))
    wire := reg.io.o
    wire.addDoc(wireDoc)

    val node = wire + 1.U
    dontTouch(node)
    node.addDoc(nodeDoc)
    io.o := node

    io.i.addAttr(attribute)
  }

  val annos = (new ChiselStage).execute(Array(), Seq(ChiselGeneratorAnnotation(() => new M(5))))
  val verilog = annos.collectFirst { case EmittedVerilogCircuitAnnotation(value) => value }.get.value

  it should "be able to create comment" in {
    assert(verilog.contains(moduleDoc))
    assert(verilog.contains(ioDoc))
    assert(verilog.contains(instanceDoc))
    assert(verilog.contains(wireDoc))
    assert(verilog.contains(nodeDoc))
    assert(verilog.contains(regDoc))
    assert(verilog.contains(attribute))
  }
}
