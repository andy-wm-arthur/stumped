/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

object neuralNet {
	def main (args:Array[String]) = {
		val data = new genMatrix("../resources/test.csv")
		val matrix: List[funVector] = data.matrix

		print(matrix)
	}
}