
import Chisel._

object Main extends App {
	val argz = Array(
		"--backend", "c", 
		"--compile", 
		"--genHarness", 
		"--test", 
		"--targetDir", "./target")
	
	//chiselMain(argz, () => Module(CompareAndSwap()))
	chiselMainTest(argz, () => Module(CompareAndSwap())){
          cas => new CompareAndSwapTester(cas)}
}

case class CompareAndSwap() extends Module {
	val io = new Bundle {
		val in0 = UInt(INPUT, width=32)
		val in1 = UInt(INPUT, width=32)

		val out0 = UInt(OUTPUT, width=32)
		val out1 = UInt(OUTPUT, width=32)
	}
	import io._

	out0 := in0
	out1 := in1

	when (in1 < in0) {
		out0 := in1
		out1 := in0
	}	
}

case class CompareAndSwapTester(cas: CompareAndSwap) extends Tester(cas)  {
	import cas.io._

	poke ( in0, 10 )
	poke ( in1, 11 )

	expect ( out0, 10)
	expect ( out1, 11)

	step(1)

	poke ( in0, 23 )
	poke ( in1, 22 )

	expect ( out0, 22)
	expect ( out1, 23)

	step(1)
}

