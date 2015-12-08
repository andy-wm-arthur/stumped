/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

import scala.io.Source
import scala.util.Random
import scala.compat.Platform
import scala.annotation.tailrec

object neuralNet {

	class metaParams(	
		/**
		 *	Struct class to encapsulate meta-parameters
		 * 	to the Neural Network
		 */
		val sigmoid: Double => Double, 
		val sigPrime: Double => Double,
		val costFunc: (funMatrix, funMatrix) => funMatrix,  
		val costPrime: (funMatrix, funMatrix) => funMatrix,
		val learningRate: Double
	) {}

	def genDataMatrix( path:String): funMatrix = {
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
	
	def genNeuralNetwork( structure: List[Int], prng: Random): Network = {
		/**
		 *	Assumes structure to be: [inputLayer, hidden layers*, outputLayer],
		 * 	with as many hidden layers as needed. A weight matrix and bias vector
		 *	will be instantiated for every non-input layer.
		 */
		@tailrec
		def genMatrix(n: Int, m: Int, aggL: List[funVector], prng: Random): List[funVector] = {
			n match {
				case 0 => aggL
				// bug: only pass it natural numbers (efficiency)
				case x => genMatrix( x-1, m, (new funVector(genVector(m,Nil,prng)) :: aggL), prng)
			}
		}
		@tailrec
		def genVector( n: Int, aggL: List[Double], prng: Random): List[Double] = {
			n match {
				case 0 => aggL
				// bug: only pass it natural numbers (efficiency)
				case x => genVector( x-1, prng.nextDouble() :: aggL, prng)
			}
		}

		def genNN_r( structure: List[Int], prng: Random): (List[funMatrix],List[funVector]) = {
			structure match {
				case x :: Nil 		=> (Nil,Nil)
				case x1 :: x2 :: xs => {
					val w = genMatrix( x1, x2, Nil, prng)
					val b = genVector( x2, Nil, prng)
					val (ws,bs) = genNN_r( x2 :: xs, prng)
					( new funMatrix(w)::ws, new funVector(b)::bs)
				}
				// bug: error case
				case _				=> (Nil,Nil)
			}	
		}
		val (weights,biases) = genNN_r( structure, prng)
		new Network( weights, biases)
	}

	def train( NN: Network, batchSize: Int, epochs: Int, trainData:funMatrix, labels:funMatrix, mp: metaParams ): Network = {
		val batches = trainData.split(batchSize)
		val labelSets = labels.split(batchSize)
		new Network(Nil,Nil)
	}

	def main (args:Array[String]) = {
		val dataPnts = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/MNIST_data/MNIST5.csv")
		val labels	 = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/labelMatrix.csv")
		
		val NN = genNeuralNetwork( List(784,28,10), new Random(Platform.currentTime))



	}
}