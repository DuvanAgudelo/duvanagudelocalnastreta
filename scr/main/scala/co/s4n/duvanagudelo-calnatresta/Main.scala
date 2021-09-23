package co.s4n.calnat

import scala.io.Stdin

trait Nat
case class Cero() extends Nat
case class Suc(nat:Nat) extends Nat

object Main extends App {
  def leerInt(prompt:Sring):Int ={
    val s = StdIn.readLine(prompt)
    s.toInt
  }
}
def esCero(nat:Nat) = nat match {
  case Cero() => true
  case Suc(Nat) => false
}
def esMayorIgual(nat1:Nat,nat2:Nat):Boolean = nat1 match {
  case Cero() => nat2 match {
    case Cero() => true
    case _ => false
  }
  case Suc(pnat) => nat2 match {
    case Cero() => true
    case Suc(snat) => es esMayorIgual(pnat,snat)
  }
}
def conIntANat(n:Int) = {
  @scala.annotation.tailrec
  def a(n:Int, acum:Nat):Nat = n match {
    case 0 => acum
    case n => a(n-1,Suc(acum))
    }
    a(n,Cero())
}

def imprimirNat(n:Int):String
  @scala.annotation.tailrec
  def a(n:Int, acum:String):String = n match {
    case 0 => acum
    case n => a(n-1,"Suc("+(acum)+")")
    }
    a(n,"Cero")
}
