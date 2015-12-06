/**
* @author Andy Arthur
* @author Ramya Puliadi
*/



class funMatrix( val vecs: List[funVector] ) {
      /**
       * M x N matrix class: N column vectors of M elems
       */
      def transMult( rhMat: funMatrix): funMatrix = {
            /** 
             *  transposes the left-hand-side matrix and then computes
             *    matrix multiplication for the two matrices  
             */
            def inner( lhMat: List[funVector], rhVec: funVector): List[Double] = {
                  /** for a right-hand col-vec: compute dot products for all
                   *    the row vecs in transpose(left-hand matrix)
                   */
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

      def transVecMult( vec: funVector): funVector = {
            /**
             *  multiplies the transpose of an M x N matrix A and 
             *    an M element vector v:  transpose(A)*v
             */
            def transVecMult_r( x: funVector, vecs: List[funVector]): List[Double] = {
                  vecs match {
                        case Nil     => Nil
                        case v :: vs => (x dot v) :: transVecMult_r( x, vs)
                  }
            }
            new funVector( transVecMult_r( vec, vecs))
      }

      def matrixMap( f: Double => Double): funMatrix = {
            def map_r( mat: List[funVector], f: Double => Double): List[funVector] = {
                  mat match {
                        case Nil     => Nil
                        case v :: vs => v.vecMap(f) :: map_r( vs, f)
                  }
            }
            new funMatrix( map_r( vecs, f))
      }

      def transpose: funMatrix = {
            def distribute( vec: List[Double], transMat: List[List[Double]]): List[List[Double]] = {
                  ( vec, transMat) match {
                        case (Nil, _)            => Nil
                        /*  puts new elements at head instead of tail: corrected below */
                        case ( v :: vs, t :: ts) => (v :: t) :: distribute( vs, ts)
                        case ( v :: vs, Nil)     => (v :: Nil) :: distribute( vs, Nil)
                  }
                  
            }
            def flip_and_vec(transMat: List[List[Double]]): List[funVector] = {
                  /**   inner lists are backwards: reverse them and convert to vectors
                   */
                  transMat match {
                        case Nil     => Nil
                        case t :: ts => (new funVector(t.reverse)) :: flip_and_vec(ts)
                  }
            }
            def trans_r( inMat: List[funVector], transMat: List[List[Double]]): List[funVector] = {
                  inMat match {
                        case Nil     => flip_and_vec(transMat)
                        case i :: is => trans_r(is, distribute( i.elems, transMat))
                  }
            }
            new funMatrix( trans_r( vecs, Nil ))
      }

      def size: (Int,Int) = {
            /**   returns a tuple: ( number-of-rows, number-of-columns) */
            def col_size( vecs: List[funVector]): Int = {
                  vecs match {
                        case Nil                => 0
                        case v1 :: v2 :: Nil    => if (v1.len == v2.len) v1.len else -1
                        case v :: vs            => if (v.len == col_size(vs)) v.len else -1
                  }
            }
            def num_rows( vecs: List[funVector]): Int = {
                  vecs match {
                        case Nil     => 0
                        case v :: vs => 1 + num_rows(vs)
                  }
                  
            }
            ( num_rows(vecs), col_size(vecs))
      }

      override def toString(): String = {
            def toStr_r( vecs: List[funVector]): String = {
                  vecs match {
                        case Nil     => ""
                        case v :: vs => v.elems.mkString(",\t") + '\n' + toStr_r(vs)
                  }
            }
            /** funMatrices are List of column vectors, but vectors print as row vectors
             *    printing the transpose of the matrix formats it correctly
             */
            toStr_r(this.transpose.vecs)
      }
}