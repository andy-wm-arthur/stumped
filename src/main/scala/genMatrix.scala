/**
* @author Andy Arthur
* @author Ramya Puliadi
* @description Reads in csv file and creates a list of list of doubles
*/

import scala.io.Source

class genMatrix(path:String) {
	val matrix: List[List[Double]] = Source.fromFile(path)
						.getLines.toList
						.map(_.split(",").map(_.trim.toDouble)
						.toList)	
}

