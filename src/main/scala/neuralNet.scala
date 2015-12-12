/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

import scala.io.Source
import scala.util.Random
import scala.compat.Platform
import scala.annotation.tailrec
import scala.math._


object neuralNet {

	// class metaParams(	
	// 	/**
	// 	 *	Struct class to encapsulate meta-parameters
	// 	 * 	to the Neural Network
	// 	 */
	// 	val sigmoid: Double => Double, 
	// 	val sigPrime: Double => Double,
	// 	val costFunc: (funMatrix, funMatrix) => funMatrix,  
	// 	val costPrime: (funMatrix, funMatrix) => funMatrix,
	// 	val learningRate: Double
	// ) {}

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
		def train_inner(NN: Network, epochs: Int, batches: List[funMatrix], 
						labelSets: List[funMatrix], mp: metaParams ): (Network,Int) = {
			/**
			 *	Iterates through epochs stopping if the epochs or training batches are exhausted. Returns the most recently
			 * 	trained network and the number of epochs left	
			 */
			(epochs,batches,labelSets) match {
				case (0,_,_) 			=> (NN,0)
				case (i,Nil,Nil)		=> (NN,i)
				case (i, b::bs, l::ls)	=>{
					println("epochs left: "+i)
					train_inner( NN.learn( b, l, mp), i-1, bs, ls, mp)
				}
				// bug: error case
				case (_,_,_)			=> { println("error in train_inner in train") ;(new Network(Nil,Nil),0) }
			}
		}
		def train_outer(NN: Network, epochs: Int, batches: List[funMatrix], 
						labelSets: List[funMatrix], mp: metaParams ): Network = {
			epochs match {
				case 0 => NN
				case i => {
					val (new_net,new_i) = train_inner( NN, i, batches, labelSets, mp)
					train_outer( new_net, new_i, batches, labelSets, mp)
				}
			}

		}

		val batches = trainData.split(batchSize)
		val labelSets = labels.split(batchSize)

		train_outer( NN, epochs, batches, labelSets, mp)
	}

	def test(): Network = {
		val sigmoid = (d:Double) => (1/(1 + exp(-d)))
		val mp = new metaParams(
			sigmoid,
			(d:Double) => sigmoid(d) * (1-sigmoid(d)),
			(m1:funMatrix,m2:funMatrix) => m1,
			(m1:funMatrix,m2:funMatrix) => m1 subtract m2,
			0.2
		)

		println("importing trainging data...")
		val dataPnts = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/MNIST_data/MNIST5.csv")
		println("importing trainging labels...")
		val labels	 = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/labelMatrix.csv")

		println("initializing network...")
		val NN 		= genNeuralNetwork( List(784,28,10), new Random(Platform.currentTime))
		println("training...")
		train( NN, 200, 600, dataPnts, labels, mp)
	}

	def main (args:Array[String]) = {

		val sigmoid = (d:Double) => (1/(1 + exp(-d)))
		val mp = new metaParams(
			sigmoid,
			(d:Double) => sigmoid(d) * (1-sigmoid(d)),
			(m1:funMatrix,m2:funMatrix) => m1,
			(m1:funMatrix,m2:funMatrix) => m1 subtract m2,
			0.2
		)

		println("importing trainging data...")
		val dataPnts = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/MNIST_data/MNIST5.csv")
		println("importing trainging labels...")
		val labels	 = genDataMatrix("/Users/andyarthur/classes/PLC/stumped/src/main/resources/labelMatrix.csv")

		println("initializing network...")
		val NN 		= genNeuralNetwork( List(784,28,10), new Random(Platform.currentTime))
		println("training...")
		val trained = train( NN, 200, 600, dataPnts, labels, mp)
		println("done!")
	}
}