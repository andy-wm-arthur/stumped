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

		 // def errorMat( ouput: funMatrix, labels: funMatrix, costFunc: (funVector, funVector) => funVector ): funMatrix = {
		 // 	new funMatrix(Nil)
		 // }

		 // def updateOutput( weights: List[funMatrix], biases: List[funVector], ) {}
		 // def updateLayer(weights: funMatrix, biases: funVector)


		 def learn_r( acts: funMatrix, layers: List[funMatrix], biases: List[funVector], mp: metaParams ) {}

		 new Network(Nil,Nil)
	}
}