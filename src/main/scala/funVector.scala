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
                        case (_,_)           => {println("error: size mismatch in funVector add"); Nil}
                  }
            }
            new funVector( add_r( elems, rhs.elems))
      }

      def subtract( rhs: funVector): funVector = {
            def sub_r( lhs: List[Double], rhs: List[Double]): List[Double] = {
                  (lhs,rhs) match {
                        case (Nil,Nil)     => Nil
                        case (l::ls,r::rs) => (l-r) :: sub_r( ls, rs)
                        case (_,_)         => {println("error: size mismatch in funVector subtract"); Nil}
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
                        case (_,_)              => {println("error: size mismatch in funVector dot"); 0}
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
                        case (_,_)              => {println("error: size mismatch in funVector hadamard"); Nil}
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

      def summarize: funVector = {
            def smrz( l: List[Double], index: Int, cnt: Int, max: Double): Int = {
                  l match {
                        case Nil     => index
                        case l :: ls => {
                              if (l > max) smrz( ls, cnt+1, cnt+1, l) 
                              else         smrz( ls, index, cnt+1, max)
                        }
                  }
            }
            def create( l: List[Double], index: Int, cnt: Int): List[Double] = {
                  l match {
                        case Nil => Nil
                        case l :: ls => (if(cnt == index) 1 else 0) :: create( ls, index, cnt+1)
                  }
            }
            val (e :: es) = this.elems
            val i = smrz( (e :: es), -1, 0, e)
            new funVector( create( e :: es, i, 0))
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

      def isEql(rhs: funVector): Boolean = {
            def isEql_r (rhs:List[Double], lhs:List[Double]) : Boolean = {
                  (rhs, lhs) match {
                        case (Nil,Nil)       => true
                        case ( l::ls, r::rs) => if (l == r) {isEql_r(ls, rs)} else {false}
                        case (_,_)           => {println("error: size mismatch in funVector isEql"); Nil}
                  }
            }
      }

      override def toString() : String = "[ " + elems.mkString(" ") + " ]"
}