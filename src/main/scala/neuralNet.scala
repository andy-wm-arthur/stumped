/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

<<<<<<< HEAD
object neuralNet {
	def main (args:Array[String]) = {
		val data = new genMatrix("../resources/test.csv")
		val matrix: List[funVector] = data.matrix

		print(matrix)
=======
class neuralNet () {
	def main (args:Array[String]) = {
		val data = new genMatrix("../resources/test.csv")
		val matrix: List[List[Double]] = data.matrix
>>>>>>> 1cfa64e4da957fd9a8db2e77af129b60be87b6b4
	}
}