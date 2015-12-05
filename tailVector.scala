/**
* @author Andy Arthur
*/

import scala.annotation.tailrec

class tailVector( val elems: List[Double] ) {
      
      def dot( rhs: tailVector): Double = {
            // tail recursive helper
            @tailrec
            def ldot( sum: Double, rl: List[Double], ll: List[Double]): Double = {
                  (rl,ll) match {
                        case (Nil,Nil)          => sum
                        case (r :: rs, l :: ls) => ldot( (sum + (r * l)), rs,ls)
                        case (_,_)              => sum 
                  }
            }
            ldot( 0, this.elems, rhs.elems)
      }

      def hadamard( rhs: tailVector): tailVector = {
            // tail recursive helper
            @tailrec
            def lhad( prod: List[Double], rl: List[Double], ll: List[Double]): List[Double] = {
                  (rl,ll) match {
                        case (Nil,Nil)      => prod
                        case (r::rs, l::ls) => lhad( (r * l)::prod, rs, ls)
                        case (_,_)              => prod // bug: tailVector length mismatch is uncaught
                  }
            }
            
            new tailVector( lhad( Nil, this.elems, rhs.elems).reverse )
      }

      def vecMap( f: Double => Double ): tailVector = {
            // tail recursive helper
            @tailrec
            def map_r( agg: List[Double], elems: List[Double], f: Double => Double): List[Double] = {
                  elems match {
                        case Nil    => Nil
                        case e::es  => map_r( f(e)::agg, es, f)
                  }
            }
            new tailVector( map_r( Nil, this.elems, f).reverse )
      }

      override def toString() : String = "[ " + elems.mkString(" ") + " ]"
}