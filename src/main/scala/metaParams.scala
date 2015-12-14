class metaParams(	
		/**
		 *	Struct class to encapsulate meta-parameters
		 * 	to the Neural Network
		 */
		val sigmoid: Double => Double, 
		val sigPrime: Double => Double,
		val costFunc: (funMatrix, funMatrix) => funMatrix,  
		val costPrime: (funMatrix, funMatrix) => funMatrix,
		var learningRate: Double
	) {}