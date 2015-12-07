/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

/**
 *    @TODO implement folder and binary map functions
 */

class funVector( val elems: List[Double] ) {

      def add( rhs: funVector): funVector = {
            def add_r( lhs: List[Double], rhs: List[Double]): List[Double] = {
                  ( lhs, rhs) match {
                        case (Nil,Nil)       => Nil
                        case ( l::ls, r::rs) => (l + r) :: add_r( ls, rs)
                        case (_,_)           => Nil
                  }
            }
            new funVector( add_r( elems, rhs.elems))
      }

      def subtract( rhs: funVector): funVector = {
            def sub_r( lhs: List[Double], rhs: List[Double]): List[Double] = {
                  (lhs,rhs) match {
                        case (Nil,Nil)     => Nil
                        case (l::ls,r::rs) => (l-r) :: sub_r( ls, rs)
                        case (_,_)         => Nil
                  }
            }
            new funVector( sub_r( this.elems, rhs.elems))
      }

      def dot( rhs: funVector): Double = {
            // recursive helper
            def dot_r(rl: List[Double], ll: List[Double]): Double = {
                  (rl,ll) match {
                        case (Nil,Nil)          => 0
                        case (r :: rs, l :: ls) => (r * l) + dot_r(rs,ls)
                        case (_,_)              => 0 
                  }
            }
            dot_r(this.elems,rhs.elems)
      }

      def hadamard( rhs: funVector): funVector = {
            // recursive helper
            def had_r(rl: List[Double], ll: List[Double]): List[Double] = {
                  (rl,ll) match {
                        case (Nil,Nil)          => Nil                        
                        case (r :: rs, l :: ls) => (r * l) :: had_r(rs,ls)
                        case (_,_)              => Nil // bug: funVector length mismatch is uncaught
                  }
            }
            new funVector( had_r(this.elems, rhs.elems) )
      }

      def dyadic( rhs: funVector): funMatrix = {
            def dyadic_r( rElems: List[Double]): List[funVector] = {
                  rElems match {
                        case Nil     => Nil
                        case e :: es => this.vecMap( (d:Double) => d*e) :: dyadic_r(es)
                  }
            }
            new funMatrix( dyadic_r(rhs.elems))
      }

      def vecMap( f: Double => Double ): funVector = {
            // recursive helper
            def map_r( elems: List[Double], f: Double => Double): List[Double] = {
                  elems match {
                        case Nil    => Nil
                        case e::es  => f(e) :: map_r(es, f)
                  }
            }
            new funVector(map_r(this.elems, f))
      }

      def len: Int = {
            def len_r( elems: List[Double]): Int = {
                  elems match {
                        case Nil => 0
                        case e :: es => 1 + len_r(es)
                  }
            }
            len_r(elems)
      }

      override def toString() : String = "[ " + elems.mkString(" ") + " ]"
}