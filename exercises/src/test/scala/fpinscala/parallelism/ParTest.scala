package fpinscala.parallelism

import org.scalatest.flatspec.AnyFlatSpec
import java.util.concurrent._

class ParTest extends AnyFlatSpec {
    "map2" should "respect timouts" in {
        val pool = Executors.newFixedThreadPool(5)
        val par1 = Par.lazyUnit({Thread.sleep(10); 10})
        val par2 = Par.lazyUnit({Thread.sleep(10); 15})
        assertThrows[TimeoutException](Par.run(pool)(Par.map2(par1, par2, 10, TimeUnit.MILLISECONDS)(_ + _)).get())
        assert(Par.run(pool)(Par.map2(par1, par2, 100, TimeUnit.MILLISECONDS)(_ + _)).get() == 25)
    }

    "sequence" should "work" in {
        val pool = Executors.newFixedThreadPool(5)
        val expected = Par.lazyUnit(List(1, 2, 3))
        val seq = List(Par.lazyUnit(1), Par.lazyUnit(2), Par.lazyUnit(3))
        assert(Par.equal(pool)(expected, Par.sequence(seq)))
    }

    "parFilter" should "work" in {
        val pool = Executors.newFixedThreadPool(5)
        val expected = Par.lazyUnit(List(1, 2, 3))
        val seq = List(1, 2, 3, 4, 5)
        assert(Par.equal(pool)(expected, Par.parFilter(seq)(_ < 4)))
    }
}
