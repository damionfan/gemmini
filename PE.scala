// See README.md for license details.
package gemmini

import chisel3._
import chisel3.util._

//scala 泛化类型
//泛化 上界
// PEControl 的参数类型必须是Data:Arithmetic的子类
//Reference: https://docs.scala-lang.org/zh-cn/tour/upper-type-bounds.html
//Function: 对PE的数据传输和传播进行控制。是其控制信号线。
class PEControl[T <: Data : Arithmetic](accType: T) extends Bundle {
  val dataflow = UInt(1.W) // TODO make this an Enum
  val propagate = UInt(1.W) // Which register should be propagated (and which should be accumulated)?
  val shift = UInt(log2Up(accType.getWidth).W) // TODO this isn't correct for Floats

}

// TODO update documentation
/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  */
class PE[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value, max_simultaneous_matmuls: Int)
                   (implicit ev: Arithmetic[T]) extends Module { // Debugging variables
  import ev._

  val io = IO(new Bundle {
    val in_a = Input(inputType)
    val in_b = Input(outputType)
    val in_d = Input(outputType)
    val out_a = Output(inputType)
    val out_b = Output(outputType)
    val out_c = Output(outputType)
// c = a*b +d //
    val in_control = Input(new PEControl(accType))
    val out_control = Output(new PEControl(accType))

    val in_id = Input(UInt(log2Up(max_simultaneous_matmuls).W))
    val out_id = Output(UInt(log2Up(max_simultaneous_matmuls).W))

    val in_last = Input(Bool())
    val out_last = Output(Bool())

    val in_valid = Input(Bool())
    val out_valid = Output(Bool())

    val bad_dataflow = Output(Bool())
  })
//Wait, what is the type of Dataflow.WS ?
// Reference: https://www.rle.mit.edu/eems/wp-content/uploads/2019/06/Tutorial-on-DNN-05-DNN-Accelerator-Architectures.pdf
// 权重固定的情况下，输入的类型是 输入的类型。如果是输出固定的情况下是，输出是out type。
//这里是不是反了？

//如果是WS模式的情况下，数据需要要写入scratchpad memory。
//因此，c的类型是inputtype,后面会被共享，作为输入使用。
//为什么是WS模式会共享C呢？
//是否是因为，在WS模式下，最后的累和操作需要accumulator完成？
  val cType = if (df == Dataflow.WS) inputType else accType


// Note that in the weight-stationary mode, an inputType D usually has insufficient bitwidth to accurately represent partial sums. Therefore, in the weight-stationary mode, D is usually just the 0-matrix, while the accType accumulator SRAMs are used to accumulate partial sum outputs of the systolic array instead.

  val a  = io.in_a
  val b  = io.in_b
  val d  = io.in_d
  val c1 = Reg(cType)
  val c2 = Reg(cType)
  val dataflow = io.in_control.dataflow //和DF的区别是什么？
  val prop  = io.in_control.propagate
  val shift = io.in_control.shift
  val id = io.in_id
  val last = io.in_last
  val valid = io.in_valid

  io.out_a := a
  io.out_control.dataflow := dataflow
  io.out_control.propagate := prop
  io.out_control.shift := shift
  io.out_id := id
  io.out_last := last
  io.out_valid := valid

// RegEnable(next, enable) update, with enable gate
// 当input valid，last_s is prop.
  val last_s = RegEnable(prop, valid)
  val flip = last_s =/= prop
  val shift_offset = Mux(flip, shift, 0.U)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  io.bad_dataflow := false.B
  when ((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && dataflow === OUTPUT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := (c1 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c2 := c2.mac(a, b.asTypeOf(inputType))
      c1 := d.withWidthOf(cType)
    }.otherwise {
      io.out_c := (c2 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c1 := c1.mac(a, b.asTypeOf(inputType))
      c2 := d.withWidthOf(cType)
    }
  }.elsewhen ((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := c1
      io.out_b := b.mac(a, c2.asTypeOf(inputType))
      c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := b.mac(a, c1.asTypeOf(inputType))
      c2 := d
    }
  }.otherwise {
    io.bad_dataflow := true.B
    //assert(false.B, "unknown dataflow")
    io.out_c := DontCare
    io.out_b := DontCare
  }

  when (!valid) {
    c1 := c1
    c2 := c2
  }
}
