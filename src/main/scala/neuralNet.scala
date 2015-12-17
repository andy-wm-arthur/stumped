/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

import scala.io._
import java.io._
import scala.util.Random
import scala.math._
import scala.compat.Platform
import scala.annotation.tailrec



object neuralNet {

	/**	@TODO
	 *	remove non-prime cost function from metaParams
	 *	make meta-params part of Network constructor
	 *	add mappings from output vectors to class string
	 *	convert functions to tail recursion
	 *	implement more efficient matrix multiplication
	 *	implement a binary map to improve funMatrix and funVector src
	 *	implement matrix dyadic sum
	 *	cleanup resource directory
	 */

	def genDataMatrix( file: InputStream): funMatrix = {
		@tailrec
		def cvtVec (agg: List[funVector], matrix:List[List[Double]]): List[funVector] = {

			matrix match {
				case Nil 	=> agg
				case l::ls 	=> cvtVec( new funVector(l) :: agg, ls)
			}
		}
		val lst2d: List[List[Double]] = Source.fromInputStream(file)
							.getLines.toList
							.map(_.split(",").map(_.trim.toDouble)
							.toList)

		new funMatrix( cvtVec( Nil, lst2d))
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
				case x => genVector( x-1, prng.nextGaussian()*0.5 :: aggL, prng)
				// case x => genVector( x-1, 0.0 :: aggL, prng)
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

	def train( NN: Network, mp: metaParams, trainData:funMatrix, labels:funMatrix): Network = {

		def train_inner(NN: Network, iterations: Int, batches: List[funMatrix], labelSets: List[funMatrix], mp: metaParams ): (Network,Int) = {
			/**
			 *	Iterates through iterations stopping if the iterations or training batches are exhausted. Returns the most recently
			 * 	trained network and the number of iterations left	
			 */
			(iterations,batches,labelSets) match {
				case (0,_,_) 			=> (NN,0)
				case (i,Nil,Nil)		=> (NN,i)
				case (i, b::bs, l::ls)	=>{
					if (i % (mp.iterations/100) == 0) println("iterations left: "+i)
					if (i % (mp.iterations/20)  == 0) evaluate( labels, NN.computeOutput( trainData, mp.sigmoid ))

					train_inner( NN.learn( b, l, mp), i-1, bs, ls, mp)
				}
				// bug: error case
				case (_,_,_)			=> { println("error in train_inner in train") ;(new Network(Nil,Nil),0) }
			}
		}
		def train_outer(NN: Network, iterations: Int, batches: List[funMatrix], 
						labelSets: List[funMatrix], mp: metaParams ): Network = {
			iterations match {
				case 0 => NN
				case i => {
					val (new_net,new_i) = train_inner( NN, i, batches, labelSets, mp)
					
					mp.learningRate = (mp.lr_scale * mp.learningRate )
					if(mp.lr_scale != 1.0) println("new learning rate: "+mp.learningRate)

					train_outer( new_net, new_i, batches, labelSets, mp)
				}
			}

		}

		val batches = trainData.split(mp.batchSize)
		val labelSets = labels.split(mp.batchSize)

		train_outer( NN, mp.iterations, batches, labelSets, mp)
	}

	def evaluate( labels:funMatrix, output:funMatrix) {
		@tailrec
		def eval_r( labels:List[funVector], output:List[funVector], correct:Int, wrong:Int) {
			(labels,output) match {
				case (Nil,Nil) 		=> println( "Classification Accuracy:\n"+ correct +" / "+ (correct+wrong))
				case (l::ls,o::os)  => {
					if ( l.isEql( o.summarize)) eval_r( ls, os, correct+1, wrong)
					else 						eval_r( ls, os, correct, wrong+1)

				}
				case (_,_) => return
			}
		}
		eval_r( labels.vecs, output.vecs, 0, 0)
	}

	def test( mp: metaParams, dataPnts: funMatrix, labels:funMatrix, testData:funMatrix, testLabels: funMatrix) {

		// var NN = genNeuralNetwork( mp.structure, new Random(Platform.currentTime))
		var NN = genNeuralNetwork( mp.structure, new Random(42))

		println("\nmeta-parameters: "+mp.batchSize+" : "+mp.iterations+" : "+mp.learningRate+" : "+mp.lr_scale+" : "+mp.structure+"\n")

		println("\ntraining...")
		NN = train( NN, mp, dataPnts, labels)

		val testOutput = NN.computeOutput( testData, mp.sigmoid )

		println("\n\nTest Set Evaluation")
		evaluate( testLabels, testOutput)
		
	}

	def main (args:Array[String]) {

		val mp = new metaParams(

			if(args.size > 0) (args(0)).toInt else 10,												//	mini-batch size
			if(args.size > 1) (args(1)).toInt else 10000,											//	iterations
			if(args.size > 2) (args(2)).toDouble else 0.2,											//	initial learning rate
			if(args.size > 3) (args(3)).toDouble else 1.0,											//	learning rate scale
			if(args.size > 4) (args(4)).split(",").toList.map(_.toInt) else List(784,28,10),		//	layer structure
			(d:Double) => (1/(1 + exp(-d))),														//	sigmoid
			(d:Double) => (1/(1 + exp(-d))) * (1-(1/(1 + exp(-d)))),								//	sigmoid prime
			(m1:funMatrix,m2:funMatrix) => m1 subtract m2											//	costPrime
		)


		println("\nimporting training data...")
		val dataPnts = genDataMatrix( getClass.getResourceAsStream("MNIST_data/first10k.csv"))
		println("importing training labels...")
		val labels	 = genDataMatrix( getClass.getResourceAsStream("MNIST_labels/first10k_labels.csv"))
		println("importing test data...")
		val testData = genDataMatrix( getClass.getResourceAsStream("MNIST_data/second10k.csv"))
		println("importing test labels...")
		val testLabels = genDataMatrix( getClass.getResourceAsStream("MNIST_labels/second10k_labels.csv"))


		test( mp, dataPnts, labels, testData, testLabels)		
	}	
}