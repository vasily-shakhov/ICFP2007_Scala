package ifcp2007

import scala.util.control.Breaks._

class DNA(d: String) {
  var dna = d
  var rna = ""

  def translate() = {
    println("Start")
    while (true) {
      val p = pattern()
      val t = template()
      matchreplace(p, t)
    }
  }

  def matchreplace(pat: String, t: String) = {
    var i = 0

    /*proc matchreplace (pat : Pattern, t : Template) =
98 let i : N ← 0
99 let e : Environment ← ε
100 let c : N∗ ← ε
101 foreach p ∈ pat
102 case p is of the form
103 b ⇒ if dna[i] = b
104 then i ← i + 1
105 else return
106 end if
107 !n ⇒ i ← i + n
108 if i > len (dna) then return end if
109 ?s ⇒ if there is a smallest n : N such that n > i and s is a postfix of dna[i . . n]
110 then i ← n
111 else return
112 end if
113 ( ⇒ c ← i C c
114 ) ⇒ e ← e B dna[c[0] . . i]; c ← c[1 . .]
115 end case
116 end foreach
117 dna ← dna[i . .]
118 replace (t,e)
119 return*/

  }

  def replace(tpl: String, e: String) = {
    val r = ""
    /*    proc replace (tpl : Template,e : Environment) =
121 let r : DNA ← ε
122 foreach t ∈ tpl
123 case t is of the form
124 b ⇒ r ← r B b
125 nl ⇒ r ← r  protect (l,e[n])
126 |n| ⇒ r ← r  asnat (len (e[n]))
127 end case
128 end foreach
129 dna ← r  dna
130 return*/
  }

  def protect(l: Int, d: String): String = {
    l match {
      case 0 => d
      case _ => protect(l-1,quote(d))
    }
  }
  
  def quote (d : String) : String = {
    d match {
      case "I" =>  "C" + quote(d.drop(1))
      case "C" =>  "F" + quote(d.drop(1))
      case "F" =>  "P" + quote(d.drop(1))
      case "P" =>  "IC" + quote(d.drop(1))
      case _ => ""
    }
  }

  def finish(): Int = {
    println("Finish")
    println(rna)
    -1
  }

  val PatternRegExp = """^(C|F|P|IC|IP|IF|IIP|IIC|IIF|III}){0,1}(.*)""".r
  def pattern(): String = {
    var lvl = 0
    var p = ""
    while (true) {
      val PatternRegExp(prefix, suffix) = dna
      dna = suffix
      prefix match {
        case "C" => p = p + "I"
        case "F" => p = p + "C"
        case "P" => p = p + "F"
        case "IC" => p = p + "P"
        case "IP" =>
          val n = nat(); p = p + "!/" + n + "/"
        case "IF" =>
          dna = dna.drop(1); val s = consts(); p = p + "?/" + "s" + "/"
        case "IIP" =>
          lvl = lvl + 1; p = p + "("
        case "IIC" | "IIF" => {
          if (lvl == 0) {
            return p
          } else {
            lvl = lvl - 1; p = p + ")"
          }
        }
        case "III" =>
          rna = rna + dna.substring(0, 7); dna = dna.drop(7);
        case _ => finish(); break;
      }
    }
    ""
  }

  val NatRegExp = """^(I|P|F|C){0,1}(.*)""".r
  def nat(): Int = {
    val NatRegExp(prefix, suffix) = dna
    dna = suffix
    prefix match {
      case "I" | "F" =>
        val n = nat(); 2 * n
      case "P" => 0
      case "C" =>
        val n = nat(); 2 * n + 1
      case _ => finish()
    }
  }

  def asnat(n: Int): String = {
    n match {
      case 0 => "P"
      case x if x % 2 == 0 => "I" + asnat(n / 2)
      case x if x % 2 == 1 => "C" + asnat(n / 2)
    }
  }

  val ConstsRegExp = """^(C|F|P|IC){0,1}(.*)""".r
  def consts(): String = {
    val ConstsRegExp(prefix, suffix) = dna
    dna = suffix
    prefix match {
      case "C" =>
        val s = consts(); "I" + s
      case "F" =>
        val s = consts(); "C" + s
      case "P" =>
        val s = consts(); "F" + s
      case "IC" =>
        val s = consts(); "P" + s
      case _ => ""
    }
  }

  val TemplateRegExp = """^(C|F|P|IC|IF|IP|IIC|IIF|IIP|III){0,1}(.*)""".r
  def template(): String = {
    var t = ""
    while (true) {
      val ConstsRegExp(prefix, suffix) = dna
      dna = suffix
      prefix match {
        case "C" => t = t + "I"
        case "F" => t = t + "C"
        case "P" => t = t + "F"
        case "IC" => t = t + "P"
        case "IF" | "IP" =>
          val l = nat(); val n = nat(); t = t + n + "/" + l + "/"
        case "IIC" | "IIF" => t
        case "IIP" =>
          val n = nat(); t = t + "|" + n + "|"
        case "III" =>
          rna = rna + dna.substring(0, 7); dna = dna.drop(7)
        case _ => finish(); break;
      }
    }
    return ""
  }

}