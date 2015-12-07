/**
* @author Andy Arthur
* @author Ramya Puliadi
* @description Reads in csv file and creates a list of list of doubles
*/

import scala.io.Source

class genMatrix(path:String) {
	val lst2d: List[List[Double]] = Source.fromFile(path)
						.getLines.toList
						.map(_.split(",").map(_.trim.toDouble)
						.toList)

	def cvtVec (matrix:List[List[Double]]): List[funVector] = {
		matrix match {
			case Nil 	=> Nil
			case l::ls 	=> new funVector(l) :: cvtVec(ls)
		}
	}

	val matrix = cvtVec(lst2d)
}

