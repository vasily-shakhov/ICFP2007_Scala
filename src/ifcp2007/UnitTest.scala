package ifcp2007

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith

class UnitTest {

  @Test def testnat0() {
    val dna = new DNA("PIIC")
    val nat = dna.nat()
    assertEquals("PIIC -> 0, IIC", 0, nat)
    assertEquals("PIIC -> 0, IIC", "IIC", dna.dna)
  }

  @Test def testnat1() {
    val dna = new DNA("CPIC")
    val nat = dna.nat()
    assertEquals("CPIC -> IC,1", 1, nat)
    assertEquals("CPIC -> IC,1", "IC", dna.dna)
  }

  @Test def testPattern1() {
    val dna = new DNA("CIIC")
    assertEquals("CIIC -> I", "I", dna.pattern())
  }

  @Test def testPattern2() {
    val dna = new DNA("IIPIPICPIICICIIF")
    assertEquals("IIPIPICPIICICIIF  -> (!/2/)P", "(!/2/)P", dna.pattern())
  }
}