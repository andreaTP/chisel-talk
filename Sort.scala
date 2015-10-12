
import Chisel._

object Main extends App {
	val argz = Array(
		"--backend", "c", 
		"--compile", 
		"--genHarness", 
		"--test", 
		"--targetDir", "./target")
	
	chiselMain(argz, () => Module(CompareAndSwap()))
	//chiselMainTest(argz, () => Module(CompareAndSwap())){
    //      cas => new CompareAndSwapTester(cas)}
}

case class CompareAndSwap() extends Module {
	val io = new Bundle {
		val in0 = UInt(INPUT, width=32)
		val in1 = UInt(INPUT, width=32)

		val out0 = UInt(OUTPUT, width=32)
		val out1 = UInt(OUTPUT, width=32)
	}
}
