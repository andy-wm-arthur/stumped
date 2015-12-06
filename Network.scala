/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

class Network( val layers: List[funMatrix], val biases: List[funVector]) {

	class metaParams(	
		/**
		 *	Struct class to encapsulate meta-parameters
		 * 	to the Neural Network
		 */
		val sigmoid: Double => Double, 
		val sigPrime: Double => Double,
		val costFunc: (funVector, funVector) => funVector,  
		val costPrime: (funVector, funVector) => funVector,
		val learningRate: Double
	) {}

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

	def learn( trainingPnts: funMatrix, labels: funMatrix, learningRate: Double, mp: metaParams): Network = {
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
		 def gradientDescent( layer: funMatrix, bias: funVector, error: funVector, z: funMatrix, LR: Double): (funMatrix,funVector) = {
		 	val weightMatrix = new funMatrix(Nil)
		 	val biasVector	 = new funVector(Nil)
		 	( weightMatrix, biasVector)
		 }
		 def layerError( layer: funMatrix, bias: funVector, z: funMatrix, sigPrime: Double => Double): funVector = {
		 	new funVector(Nil)
		 }

		 def learn_r( z: funMatrix, layers: List[funMatrix], biases: List[funVector], 
		 				labels: funMatrix, mp: metaParams ): (List[funMatrix], List[funVector], funVector) = {
		 	/**
		 	 *	Recursive loop that trains the network. Takes the network, training data, and meta parameters as inputs.
		 	 *	Returns the new weight matrices and bias vectors of the trained network, as well as the error for the most 
		 	 *	recent layer trained. The error is a vector of errors for each node in the layer.
		 	 */
		 	(layers,biases) match {
		 		// case ( l::Nil, b::Nil) 	=>  // calculate error, update output layer, final weight layer
		 		case ( l::ls, b::bs) 	=> {
		 			val newZ		 	= layerProp( z, l, b, mp.sigmoid)
		 			val ( wL, bL, err)	= learn_r( newZ, ls, bs, labels, mp)
		 			val ( wM, bV)		= gradientDescent( l, b, err, z, mp.learningRate)
		 			( wM :: wL, bV :: bL, layerError( l, b, z, mp.sigPrime))
		 		}
		 		case (_,_)				=> (List(),List(),new funVector(Nil))	// add error message
		 	}
		 }

		 new Network(Nil,Nil)
	}
}