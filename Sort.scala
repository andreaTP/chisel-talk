
import Chisel._

object Main extends App {
	val argz = Array(
		"--backend", "c", 
		"--compile", 
		"--genHarness", 
		"--test", 
		"--targetDir", "./target")
	
	//chiselMain(argz, () => Module(CompareAndSwap()))
	chiselMainTest(argz, () => Module(CompareAndSwap(32))){
          cas => new CompareAndSwapTester(cas)}
}

case class CompareAndSwap(size: Int = 32) extends Module {
	val io = new Bundle {
		val in0 = UInt(INPUT, width=size)
		val in1 = UInt(INPUT, width=size)

		val out0 = UInt(OUTPUT, width=size)
		val out1 = UInt(OUTPUT, width=size)
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

	import scala.util.Random
	import scala.math.{ min, max }
	for (_ <- 0 until 100) {

		val rnd0 = Random.nextInt(Int.MaxValue)
		val rnd1 = Random.nextInt(Int.MaxValue)

		poke( in0, rnd0 )
		poke( in1, rnd1 )

		expect( out0, min( rnd0, rnd1 ))
		expect( out1, max( rnd0, rnd1 ))

		step(1)
	}
}

