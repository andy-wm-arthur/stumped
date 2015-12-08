/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

class Network( val layers: List[funMatrix], val biases: List[funVector]) {

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

	def forwardProp( dataPnt: funVector, sigmoid: Double => Double): funVector = {
		/**
		 * 	Computes the output for a single input data point
		 *		a(l+1) = sigmoid( transpose(w(l+1)) * a(l) + bias(l+1))
		 */
		def fprop_r( layers: List[funMatrix], biases: List[funVector], 
					 activations: funVector, sigmoid: Double => Double): funVector = {

			(layers, biases) match {
				//	output layer
				case (Nil,Nil) 		=> activations
				//	hidden layer
				case ( l::ls, b::bs) => {
					val a = ((l transVecMult activations) add b).vecMap(sigmoid)
					fprop_r( ls, bs, a, sigmoid )
				}
				//	this is an error case
				case (_,_)	=> { println("error: weight, bias mismatch"); new funVector(List())}
			}
		}
		fprop_r( this.layers, this.biases, dataPnt, sigmoid)
	}

	def learn( trainingPnts: funMatrix, labels: funMatrix, mp: metaParams): Network = {
		/**
		 *	Takes a matrix of training points, a matrix of their labels, sigmoid and cost functions
		 *		Forward propagates the training points together, computing an ouput matrix. Computes
		 *		an error matrix using the labels. Back propagates the error, updating the weight matrices
		 *		using the learning rate. Returns a new network learned on the triaing points
		 */

		 def layerProp(z: funMatrix, weight: funMatrix, bias: funVector, sigmoid: Double => Double): funMatrix = {
		 	val acts = z.matVecMap(sigmoid)	//	transform the raw output of the previous layer with the sigmoid
		 	(acts transMult weight).matMap( (v: funVector) => v add bias)
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
			 			case (_,_)				=> new funMatrix(Nil)	//	error case
			 		}
			 	}
			 	d_sum_r( error.vecs, actMat.vecs)
		 	}
		 	def matrix_sum( mat: funMatrix): funVector = {
		 		def mat_sum_r( mat: List[funVector]): funVector = {
		 			mat match {
		 				case m :: Nil => m
		 				case m :: ms  => m add mat_sum_r(ms)
		 				case Nil 	  => new funVector(Nil)
		 			}
		 		}
		 		mat_sum_r(mat.vecs)
		 	}
		 	val a = z.matVecMap(mp.sigmoid)
		 	val weightMatrix = layer subtract (dyadic_sum( error, a).matVecMap( (d:Double) => mp.learningRate * d))
			val biasVector 	 = bias subtract (matrix_sum(error)).vecMap( (d:Double) => mp.learningRate * d)

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
		 		case (_,_)				=> (List(),List(),new funMatrix(Nil))	// add error message
		 	}
		 }

		 val ( newLayers, newBiases, err) = learn_r( trainingPnts, this.layers, this.biases, labels, mp)
		 new Network( newLayers, newBiases)
	}
}