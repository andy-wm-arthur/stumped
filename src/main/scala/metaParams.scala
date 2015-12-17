class metaParams(	
		/**
		 *	Struct class to encapsulate meta-parameters
		 * 	to the Neural Network
		 */

		val batchSize: Int,
		val iterations: Int,
		var learningRate: Double,
		val	lr_scale: Double,
		val structure: List[Int],
		val sigmoid: Double => Double, 
		val sigPrime: Double => Double,
		val costPrime: (funMatrix, funMatrix) => funMatrix

	) {}