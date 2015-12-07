/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

import scala.io.Source

object neuralNet {

	def genMatrix( path:String): funMatrix = {
		def cvtVec (matrix:List[List[Double]]): List[funVector] = {
			matrix match {
				case Nil 	=> Nil
				case l::ls 	=> new funVector(l) :: cvtVec(ls)
			}
		}
		val lst2d: List[List[Double]] = Source.fromFile(path)
							.getLines.toList
							.map(_.split(",").map(_.trim.toDouble)
							.toList)

		new funMatrix( cvtVec(lst2d))
	}
	

	def main (args:Array[String]) = {
		val data: funMatrix = genMatrix("../resources/test.csv")
		

		print(data)
	}
}