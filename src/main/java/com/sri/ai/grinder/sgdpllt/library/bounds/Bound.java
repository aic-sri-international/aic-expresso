package com.sri.ai.grinder.sgdpllt.library.bounds;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;

public interface Bound extends Expression{
	

	/**
	 * Assumes that each element of the bound is a factor with the same domain
	 * Normalizes each factor of the bound. In latex notation: 
	 * 			{\phi/sum_{var(\phi)}\phi : \phi in bound} 
	 * @param bound
	 * @param theory
	 * @param context
	 * @return  bound of normalized factors
	 */
	public Bound normalize(Theory theory, Context context);
		
	/**
	 * given a set of variables "S" and a bound "B", performs the following operation:
	 * sum_S B = {sum_S \phi : \phi in B} 
	 * 
	 * @param variablesToBeSummedOut 
	 * 		S in the example. 
	 * 		Must be a Explicit UniSet. For example: {A,B,C} , where A, B and C are variables 
	 * @param bound
	 * 		B in the example
	 * @param context
	 * @param theory
	 * @return
	 */
	public Bound summingBound(Expression variablesToBeSummedOut,
			Context context, Theory theory);
	
	public Bound summingBound(ArrayList<Expression> variablesToBeSummedOut,
			Context context, Theory theory);
	
	/**
	 * given a set of variables "S" a factor \phi and a bound "B", performs the following operation:
	 * sum_S (\phi * B) = {sum_S \phi \phi' : \phi' in B} 
	 * 
	 * @param variablesToBeSummedOut 
	 * 		S in the example
	 * @param phi
	 * @param bound
	 * 		B in the example
	 * @param context
	 * @param theory
	 * @return
	 */
	public Bound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi,
			Context context, Theory theory);

	public Bound summingPhiTimesBound(ArrayList<Expression> variablesToBeSummedOut, Expression phi,
			Context context, Theory theory);

	public boolean isExtensionalBound();
	public boolean isIntensionalBound();
	
	/**
	 * Computes the product of each term of a list of bounds
	 * @param theory
	 * @param context
	 * @param listOfBounds
	 * @return bound resulting from the product of bounds
	 */
	//public Bound applyFunctionToBound(Expression f, Expression variableName, Bound bound, Theory theory, Context context);
	
	
//	/** Returns an explicit representation for the simplex. The expression returned is
//	 * a UniSet.
//	 * @param Variables
//	 * @param model
//	 * @return
//	 */
//	public Bound simplex(List<Expression> Variables, Model model);
//	
//	/** Returns an explicit representation for the simplex. The expression returned is
//	 * a UniSet.
//	 * @param Variables
//	 * @param model
//	 * @return
//	 */
//	public Bound simplex(List<Expression> Variables, Theory theory, Context context);
//	
//	/**
//	 * Computes the product of each term of a list of bounds
//	 * @param theory
//	 * @param context
//	 * @param listOfBounds
//	 * @return bound resulting from the product of bounds
//	 */
//	public Bound boundProduct(Theory theory, Context context, Bound...listOfBounds);
}
