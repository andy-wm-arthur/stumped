/**
* @author Andy Arthur
* @author Ramya Puliadi
*/

class Network( val layers: List[funMatrix], val biases: List[funVector]) {

	def forwardProp( dataPnt: funVector, sigmoid: Double => Double): funVector = {
		/**
		 *	a(l+1) = sigmoid( transpose(w(l+1)) * a(l) + bias(l+1))
		 */
		def fprop_r( layers: List[funMatrix], biases: List[funVector], activations: funVector, sigmoid: Double => Double): funVector = {
			(layers, biases) match {
				//	output layer
				case (Nil,Nil) 		=> activations
				//	hidden layer
				case ( l::ls, b::bs) => {
					val a = ((l transVecMult activations) add b).vecMap(sigmoid)
					fprop_r( ls, bs, a, sigmoid )
				}
				//	this is an error case
				case (_,_)	=> new funVector(List())
			}
		}
		fprop_r( this.layers, this.biases, dataPnt, sigmoid)
	}

	// backProp
		// error, learning rate

	// where to calculate error?  in highest class?
}