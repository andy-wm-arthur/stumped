/**
* @author Andy Arthur
*/

import scala.util.Random

class Vector( val elems: List[Double] ) {

      def this( gen: Random, len: Int) {
            this( List.fill(len)(gen.nextDouble()))
      }

      override def toString() : String = "[ " + elems.mkString(" ") + " ]"

      // dot product
      def dot( rhs: Vector): Double = {
            // recursive helper
            def ldot(rl: List[Double], ll: List[Double]): Double = {
                  (rl,ll) match {
                        case (r :: rs, l :: ls) => (r * l) + ldot(rs,ls)
                        case (Nil,Nil)          => 0
                        case (_,_)              => 0  // bug: vector length mismatch is uncaught
                  }
            }
            ldot(this.elems,rhs.elems)
      }

      // hadamard product
      def hadamard( rhs: Vector): Vector = {
            // recursive helper
            def lhad(rl: List[Double], ll: List[Double]): List[Double] = {
                  (rl,ll) match {
                        case (r :: rs, l :: ls) => (r * l) :: lhad(rs,ls)
                        case (Nil,Nil)          => Nil
                        case (_,_)              => Nil // bug: vector length mismatch is uncaught
                  }
            }
            //new Vector( lhad(this.elems, rhs.elems) )
            rhs
      }

}