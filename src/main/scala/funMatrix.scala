/**
* @author Andy Arthur
* @author Ramya Puliadi
*/


/**	@TODO 
 *	implement folder and binary map functions
 *	format doubles in matrix prn
 */

import scala.annotation.tailrec

class funMatrix( val vecs: List[funVector] ) {
	/**
	* M x N matrix class: N column vectors of M elems
	*/
	def multiply( rhMat: funMatrix): funMatrix = {
		this.transpose transMult rhMat
	}

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

	def matVecMap( f: Double => Double): funMatrix = {
		def map_r( mat: List[funVector], f: Double => Double): List[funVector] = {
			mat match {
			case Nil     => Nil
			case v :: vs => v.vecMap(f) :: map_r( vs, f)
			}
		}
		new funMatrix( map_r( vecs, f))
	}

	def matMap( f: funVector => funVector): funMatrix = {
		@tailrec
		def map_r( agg:List[funVector], mat: List[funVector], f: funVector => funVector): List[funVector] = {
			mat match {
				case Nil 	 => agg
				case v :: vs => map_r( f(v) :: agg, vs, f)
			}
		}
		new funMatrix( map_r( Nil, vecs, f))
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

	def add( rhMat: funMatrix): funMatrix = {
		def add_r( lhMat: List[funVector], rhMat: List[funVector]): List[funVector] = {
			(lhMat,rhMat) match {
				case (Nil,Nil) 		=> Nil
				case (l::ls,r::rs)	=> (l add r) :: add_r(ls,rs)
				case (_,_)		=> { println("error: size mismatch in funMatrix add"); Nil}
			}
		}
		new funMatrix( add_r( this.vecs, rhMat.vecs))
	}

	def subtract( rhMat: funMatrix): funMatrix = {
		def sub_r( lhMat: List[funVector], rhMat: List[funVector]): List[funVector] = {
			(lhMat,rhMat) match {
				case (Nil,Nil) 		=> Nil
				case (l::ls,r::rs)	=> (l subtract r) :: sub_r(ls,rs)
				case (_,_)		=> {println("error: size mismatch in funMatrix subtract"); Nil}
			}
		}
		new funMatrix( sub_r( this.vecs, rhMat.vecs))
	}

	def hadamard( rhMat: funMatrix): funMatrix = {
		def had_r( lhMat: List[funVector], rhMat: List[funVector]): List[funVector] = {
			(lhMat,rhMat) match {
				case (Nil,Nil) 		=> Nil
				case (l::ls,r::rs)	=> (l hadamard r) :: had_r(ls,rs)
				case (_,_)		=> {println("error: size mismatch in funMatrix hadamard"); Nil}
			}
		}
		new funMatrix( had_r( this.vecs, rhMat.vecs))
	}

	def split( size: Int): List[funMatrix] = {
		def mk_split( size: Int, vecs: List[funVector]): (List[funVector],List[funVector]) = {
			(size,vecs) match {
				case (0,vs) => (Nil,vs)
				case ( x, v :: vs) => {
					val (split,whole) = mk_split( x-1, vs)
					( v::split, whole)
				}
				case ( x, Nil) => (Nil,Nil)
			}
		}
		def split_r( size: Int, vecs: List[funVector]): List[funMatrix] = {
			vecs match {
				case Nil 	 => Nil
				case vs => {
					val(split,whole) = mk_split( size, vs)
					new funMatrix(split) :: split_r(size,whole)
				}
			}
		}
		split_r( size, vecs)
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
		def num_cols( vecs: List[funVector]): Int = {
			vecs match {
				case Nil     => 0
				case v :: vs => 1 + num_cols(vs)
			}
		}
		//	M x N
		( col_size(vecs), num_cols(vecs) )
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