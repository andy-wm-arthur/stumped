/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

class Network( val layers: List[funMatrix], val biases: List[funVector]) {


	def computeOutput( dataPnt: funVector, sigmoid: Double => Double): funVector = {
		/**
		 * 	Computes the output for a single input data point
		 *		a(l+1) = sigmoid( w(l+1) * a(l) + bias(l+1))
		 */
		def cmptOut_r( layers: List[funMatrix], biases: List[funVector], 
					 activations: funVector, sigmoid: Double => Double): funVector = {

			(layers, biases) match {
				//	output layer
				case (Nil,Nil) 		=> activations
				//	hidden layer
				case ( l::ls, b::bs) => {
					val a = ((l transVecMult activations) add b).vecMap(sigmoid)
					cmptOut_r( ls, bs, a, sigmoid )
				}
				//	this is an error case

				case (_,_)	=> { println("error: weight, bias mismatch in forwardProp()"); new funVector(List())}
			}
		}
		cmptOut_r( this.layers, this.biases, dataPnt, sigmoid)
	}

	def computeOutput( dataMat: funMatrix, sigmoid: Double => Double): funMatrix = {
		/**
		 * 	Computes the output for a matrix of data points
		 *		a(l+1) = sigmoid( w(l+1) * a(l) + bias(l+1))
		 */
		def cmptOut_r( layers: List[funMatrix], biases: List[funVector], 
					 activations: funMatrix, sigmoid: Double => Double): funMatrix = {

			(layers, biases) match {
				//	output layer
				case (Nil,Nil) 		=> activations
				//	hidden layer
				case ( l::ls, b::bs) => {
					println("\n"+l+"\n")
					val a = ((l multiply activations).matMap( (v: funVector) => v add b)).matVecMap(sigmoid)
					cmptOut_r( ls, bs, a, sigmoid )
				}
				//	this is an error case
				case (_,_)	=> { println("error: weight, bias mismatch in forwardProp()"); new funMatrix(Nil)}
			}
		}
		cmptOut_r( this.layers, this.biases, dataMat, sigmoid)
	}

	def learn( trainingPnts: funMatrix, labels: funMatrix, mp: metaParams): Network = {
		/**
		 *	Takes a matrix of training points, a matrix of their labels, sigmoid and cost functions
		 *		Forward propagates the training points together, computing an ouput matrix. Computes
		 *		an error matrix using the labels. Back propagates the error, updating the weight matrices
		 *		using the learning rate. Returns a new network learned on the training points
		 */

		 // m = number of training points
		 val (_,m) = trainingPnts.size

		 def layerProp(z: funMatrix, weight: funMatrix, bias: funVector, sigmoid: Double => Double): funMatrix = {
		 	val acts = z.matVecMap(sigmoid)	//	transform the raw output of the previous layer with the sigmoid
		 	( weight multiply acts ).matMap( (v: funVector) => v add bias)
		 }

		 def gradientDescent( layer: funMatrix, bias: funVector, error: funMatrix, z: funMatrix, mp: metaParams): (funMatrix,funVector) = {
		 	/**
		 	 *	Updates the weight and bias matrices by computing the gradient of the cost function and updating
		 	 */
		 	def dyadic_sum( error: funMatrix, actMat: funMatrix): funMatrix = {
		 		def d_sum_r( error: List[funVector], act: List[funVector]): funMatrix = {
			 		(error,act) match {
			 			case (e:: Nil,a :: Nil)	=> (e dyadic a)
			 			case (e :: es,a :: as) 	=> (e dyadic a) add (d_sum_r( es, as))
			 			case (_,_)				=> { println("error in dyadic_sum in gradientDescent"); new funMatrix(Nil)}	//	error case
			 		}
			 	}
			 	d_sum_r( error.vecs, actMat.vecs)
		 	}
		 	def matrix_sum( mat: funMatrix): funVector = {
		 		def mat_sum_r( mat: List[funVector]): funVector = {
		 			mat match {
		 				case m :: Nil => m
		 				case m :: ms  => m add mat_sum_r(ms)
		 				case Nil 	  => { println("error in matrix_sum in gradientDescent"); new funVector(Nil)}
		 			}
		 		}
		 		mat_sum_r(mat.vecs)
		 	}
		 	val a = z.matVecMap(mp.sigmoid)
		 	val weightMatrix = layer subtract (dyadic_sum( error, a).matVecMap( (d:Double) => mp.learningRate * d / m))
			val biasVector 	 = bias subtract (matrix_sum(error)).vecMap( (d:Double) => mp.learningRate * d / m)

		 	( weightMatrix, biasVector)
		 }

		 def learn_r( z: funMatrix, layers: List[funMatrix], biases: List[funVector], 
		 				labels: funMatrix, mp: metaParams ): (List[funMatrix], List[funVector], funMatrix) = {
		 	/**
		 	 *	Recursive loop that trains the network. Takes the network, training data, and meta parameters as inputs.
		 	 *	Returns the new weight matrices and bias vectors of the trained network, as well as the error for the most 
		 	 *	recent layer trained. The error is a matrix with one error vector for each training point. Each error
		 	 * 	vector has an error value for each node in a layer. 
		 	 */
		 	(layers,biases) match {
		 		// case ( l::Nil, b::Nil) 	=>  
		 		case (Nil,Nil)			=> {
		 			val a = z.matVecMap(mp.sigmoid)
		 			(Nil,Nil, mp.costPrime(a,labels) hadamard z.matVecMap(mp.sigPrime) )
		 		}
		 		case ( l::ls, b::bs) 	=> {
		 			val newZ		 	= layerProp( z, l, b, mp.sigmoid)
		 			val ( wL, bL, err)	= learn_r( newZ, ls, bs, labels, mp)
		 			val ( wM, bV)		= gradientDescent( l, b, err, z, mp)
		 			val layerError		= (l transMult err) hadamard z.matVecMap(mp.sigPrime)
		 			( wM :: wL, bV :: bL, layerError)
		 		}
		 		case (_,_)				=> { println("error in learn_r"); (List(),List(),new funMatrix(Nil))}	// add error message
		 	}
		 }

		 val ( newLayers, newBiases, err) = learn_r( trainingPnts, this.layers, this.biases, labels, mp)
		 new Network( newLayers, newBiases)
	}
}