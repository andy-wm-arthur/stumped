/**
* @author Andy Arthur
*/


/**
 * M x N matrix class: N column vectors of M elems
 *    
 */
class funMatrix( val vecs: List[funVector] ) {

      // transverse(A) * B
      def transMult( rhMat: funMatrix ): funMatrix = {
            /** for a right-hand col-vec: compute dot products for all
             *    the row vecs in transverse(left-hand matrix)
             */
            def inner( lhMat: List[funVector], rhVec: funVector): List[Double] = {
                  lhMat match {
                        case Nil     => Nil
                        case l :: ls => (l dot rhVec) :: inner( ls, rhVec)
                  }
                  
            }
            
            def outer( lhMat: List[funVector], rhMat: List[funVector]): List[funVector] = {
                  rhMat match {
                        case Nil     => Nil
                        case r :: rs => new funVector(inner( lhMat, r)) :: outer(lhMat, rs)
                  }
                  
            }
            new funMatrix( outer( this.vecs, rhMat.vecs))
      }


}