package com.sri.ai.grinder.sgdpllt.library.bounds;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;

public class Bounds{
	static boolean debug = false;
	static DefaultExtensionalBound extensionalBound = new DefaultExtensionalBound();
	static DefaultIntensionalBound intensionalBound = new DefaultIntensionalBound();
	
	/**
	 * Returns an explicit representation for the simplex. The expression returned is
	 * a UniSet.
	 * @param Variables
	 * @param model
	 * @return
	 */
	public static Bound simplex(List<Expression> Variables, Model model, boolean ExtensionalRepresentation){
		Bound result = ExtensionalRepresentation ? extensionalBound.simplex(Variables, model) : intensionalBound.simplex(Variables, model);
		return result;
	}
	
	public static Bound simplex(List<Expression> Variables, Theory theory, Context context, boolean ExtensionalRepresentation){
		Bound result = ExtensionalRepresentation ? extensionalBound.simplex(Variables, theory, context) : intensionalBound.simplex(Variables, theory, context);
		return result;
	}

	/**
	 * Assumes that each element of the bound is a factor with the same domain
	 * Normalizes each factor of the bound. In latex notation: 
	 * 			{\phi/sum_{var(\phi)}\phi : \phi in bound} 
	 * @param bound
	 * @param theory
	 * @param context
	 * @return  bound of normalized factors
	 */
	public static Bound normalize(Bound bound, Theory theory, Context context){
		Bound result;
		if(bound.isExtensionalBound()){
			result = extensionalBound.normalize(bound, theory, context);
		}
		else if(bound.isIntensionalBound()){
			result = intensionalBound.normalize(bound, theory, context);
		}
		else{
			result = null;
		}
		return result;
	}
	
	/**
	 * Computes the product of each term of a list of bounds
	 * @param theory
	 * @param context
	 * @param listOfBounds
	 * @return bound resulting from the product of bounds
	 */
	public static Bound boundProduct(Theory theory, Context context, Bound...listOfBounds){
		if(listOfBounds.length == 0){
			return null;
		}
		if(listOfBounds[0].isExtensionalBound()){
			for(Bound bound : listOfBounds){
				if(!bound.isExtensionalBound()){
					return null;
				}
			}
			Bound result = extensionalBound.boundProduct(theory, context, listOfBounds);
			return result;
		}
		if(listOfBounds[0].isIntensionalBound()){
			for(Bound bound : listOfBounds){
				if(!bound.isIntensionalBound()){
					return null;
				}
			}
			Bound result = intensionalBound.boundProduct(theory, context, listOfBounds);
			return result;
		}
		return null;
	}
	
	/**
	 * Eliminate factors not in Ext(C.Hull(B)) 
	 * @param B
	 * @return 
	 */
	public static Bound updateExtremes(Bound B,Theory theory, Context context){
		if(!B.isExtensionalBound()){
			return null;
		}
		Bound result = DefaultExtensionalBound.updateExtremes(B, theory, context);
		return result;
	}
	
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
	public static Bound summingBound(Expression variablesToBeSummedOut, Bound bound,
			Context context, Theory theory){
		Bound result = null; 
		if(bound.isExtensionalBound()){
			result = extensionalBound.summingBound(variablesToBeSummedOut, bound, context, theory);
		}
		else if(bound.isIntensionalBound()){
			result = intensionalBound.summingBound(variablesToBeSummedOut, bound, context, theory);
		}								
		return result;
	}
	
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
	public static Bound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound,
			Context context, Theory theory){
		Bound result = null; 
		if(bound.isExtensionalBound()){
			result = extensionalBound.summingPhiTimesBound(variablesToBeSummedOut, phi, bound, context, theory);
		}
		else if(bound.isIntensionalBound()){
			result = intensionalBound.summingPhiTimesBound(variablesToBeSummedOut, phi, bound, context, theory);
		}								
		return result;		
	}
	
	public static Bound summingPhiTimesBound(ArrayList<Expression> variablesToBeSummedOut, Expression phi, Bound bound,
			Context context, Theory theory){
		Expression setOfVariablesToBeSummedOut = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		Bound result = summingPhiTimesBound(setOfVariablesToBeSummedOut, phi, bound, context, theory);
		return result;
	}
	public static Bound summingBound(ArrayList<Expression> variablesToBeSummedOut, Bound bound,
			Context context, Theory theory){
		Expression setOfVariablesToBeSummedOut = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		Bound result = summingBound(setOfVariablesToBeSummedOut, bound, context, theory);
		return result;
	}
}
