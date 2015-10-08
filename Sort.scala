
import Chisel._

object Main extends App {
	val argz = Array(
		"--backend", "c", 
		"--compile", 
		"--genHarness", 
		"--test", 
		"--targetDir", "./target")
	/*
	val argvz = Array(
		"--backend", "v",
		"--compile",
		"--targetDir", "./target")
	*/
	//chiselMainTest(argz, () => Module(CompareAndSwap())){
    //      cas => new CompareAndSwapTester(cas)}

	//chiselMainTest(argz, () => Module(SortStep(6, true))){
    //      sorts => new SortStepTester(sorts)}

	//chiselMainTest(argz, () => Module(SortStep(6, false))){
    //      sorts => new SortStepTester(sorts)}		

	//chiselMainTest(argz, () => Module(Sort(6))){
    //      sort => new SortTester(sort)}	

	chiselMainTest(argz, () => Module(Sort(10))){
          sort => new SortTester(sort)}


	//chiselMain(argvz, () => Module(Sort(10)))
    
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

case class SortStep(size: Int, even: Boolean) extends Module {
	val io = new Bundle {
		val in = Vec(for (_ <- 0 until size) yield UInt(INPUT, width=32))

		val out = Vec(for (_ <- 0 until size) yield UInt(OUTPUT, width=32))
	}
	import io._

	if (even) {
		val cas = for (_ <- 0 until (size/2)) yield Module(CompareAndSwap())

		for (i <- 0 until (size/2)) {
			cas(i).io.in0 := in(2*i)
			cas(i).io.in1 := in(2*i+1)
		}

		for (i <- 0 until (size/2)) {
			out(2*i) := cas(i).io.out0
			out(2*i+1) := cas(i).io.out1
		}
	} else {
		val cas = for (_ <- 1 until (size/2)) yield Module(CompareAndSwap())

		for (i <- 1 until (size/2)) {
			cas(i-1).io.in0 := in(2*i-1)
			cas(i-1).io.in1 := in(2*i)
		}

		out(0) := in(0)
		for (i <- 1 until (size/2)) {
			out(2*i-1) := cas(i-1).io.out0
			out(2*i) := cas(i-1).io.out1
		}
		out(size-1) := in(size-1)
	}
}

case class SortStepTester(sorts: SortStep) extends Tester(sorts) {
	import sorts.io._
	if (sorts.size == 6 && sorts.even) {

		poke( in(0), 6 )
		poke( in(1), 5 )
		poke( in(2), 4 )
		poke( in(3), 3 )
		poke( in(4), 2 )
		poke( in(5), 1 )

		expect( out(0), 5 )
		expect( out(1), 6 )
		expect( out(2), 3 )
		expect( out(3), 4 )
		expect( out(4), 1 )	
		expect( out(5), 2 )	

		step(1)

	} else if (sorts.size == 6 && !sorts.even) {

		poke( in(0), 6 )
		poke( in(1), 5 )
		poke( in(2), 4 )
		poke( in(3), 3 )
		poke( in(4), 2 )
		poke( in(5), 1 )

		expect( out(0), 6 )
		expect( out(1), 4 )
		expect( out(2), 5 )
		expect( out(3), 2 )
		expect( out(4), 3 )	
		expect( out(5), 1 )	

		step(1)
	}

}

case class Sort(size: Int) extends Module {
	val io = new Bundle {
		val in = Vec(for (_ <- 0 until size) yield UInt(INPUT, width=32))

		val out = Vec(for (_ <- 0 until size) yield UInt(OUTPUT, width=32))
	}
	import io._

	val steps = for (i <- 0 until size) yield Module(SortStep(size, (i%2)==0))

	for (i <- 0 until size) {
		steps(0).io.in(i) := in(i)
	}

	for (k <- 1 until size)
		for (i <- 0 until size) 
			steps(k).io.in(i) := steps(k-1).io.out(i)

	for (i <- 0 until size)
		out(i) := steps(size-1).io.out(i)
}

case class SortTester(sort: Sort) extends Tester(sort) {
	import sort.io._
/*
	if (sort.size == 6) {
		poke ( in(0), 6 )
		poke ( in(1), 5 )
		poke ( in(2), 4 )
		poke ( in(3), 3 )
		poke ( in(4), 2 )
		poke ( in(5), 1 )

		expect ( out(0), 1 )
		expect ( out(1), 2 )
		expect ( out(2), 3 )
		expect ( out(3), 4 )
		expect ( out(4), 5 )
		expect ( out(5), 6 )
	}
*/

	for (_ <- 0 until 100) {
		import scala.util.Random
		val values = for (_ <- 0 until sort.size) yield Random.nextInt(Int.MaxValue)
		val sorted = values.sorted

		for (i <- 0 until sort.size)
			poke( in(i), values(i))

		for (i <- 0 until sort.size)
			expect( out(i), sorted(i))

		step(1)
	}
}