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
}
