package com.sri.ai.grinder.library.bounds;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;

public interface Bound extends Expression {
	
	/**
	 * Assumes that each element of the bound is a factor with the same domain
	 * Normalizes each factor of the bound. In latex notation: 
	 * 			{\phi/sum_{var(\phi)}\phi : \phi in bound} 
	 * @param bound
	 * @param theory
	 * @param context
	 * @return bound of normalized factors
	 */
	Bound normalize(Theory theory, Context context);
		
	/**
	 * Given a set of variables "S" and a bound "B", performs the following operation:
	 * sum_S B = {sum_S \phi : \phi in B} 
	 * 
	 * @param variablesToBeSummedOut 
	 * 		S in the mathematical description. 
	 * 		Must be an extensional uniset, for example {A,B,C}, where A, B and C are variables 
	 * @param bound
	 * 		B in the mathematical description
	 * @param context
	 * @param theory
	 * @return
	 */
	Bound sumOut(Expression variablesToBeSummedOut, Context context, Theory theory);
	
	Bound sumOut(ArrayList<Expression> variablesToBeSummedOut, Context context, Theory theory);
	
	/**
	 * Given a set of variables "S" a factor \phi and a bound "B", performs the following operation:
	 * sum_S (\phi * B) = {sum_S \phi \phi' : \phi' in B} 
	 * 
	 * @param variablesToBeSummedOut 
	 * 		S in the mathematical description
	 * @param phi
	 * @param bound
	 * 		B in the mathematical description
	 * @param context
	 * @param theory
	 * @return
	 */
	Bound sumOutProductByFactor(Expression variablesToBeSummedOut, Expression phi, Context context, Theory theory);

	Bound sumOutProductByFactor(ArrayList<Expression> variablesToBeSummedOut, Expression phi, Context context, Theory theory);

	boolean isExtensionalBound();
	
	boolean isIntensionalBound();
	
	/**
	 * Computes the product of each term of a list of bounds
	 * @param theory
	 * @param context
	 * @param listOfBounds
	 * @return bound resulting from the product of bounds
	 */
	// Bound applyFunctionToBound(Expression f, Expression variableName, Bound bound, Theory theory, Context context);
	
	
//	/** Returns an explicit representation for the simplex. The expression returned is
//	 * a UniSet.
//	 * @param Variables
//	 * @param model
//	 * @return
//	 */
//	Bound simplex(List<Expression> Variables, Model model);
//	
//	/** Returns an explicit representation for the simplex. The expression returned is
//	 * a UniSet.
//	 * @param Variables
//	 * @param model
//	 * @return
//	 */
//	Bound simplex(List<Expression> Variables, Theory theory, Context context);
//	
//	/**
//	 * Computes the product of each term of a list of bounds
//	 * @param theory
//	 * @param context
//	 * @param listOfBounds
//	 * @return bound resulting from the product of bounds
//	 */
//	Bound boundProduct(Theory theory, Context context, Bound...listOfBounds);
}
