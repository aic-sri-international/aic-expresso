package com.sri.ai.grinder.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.anytime.Model;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;

public class Bounds{
	static boolean debug = false;
//	static DefaultExtensionalBound extensionalBound = new DefaultExtensionalBound();
//	static DefaultIntensionalBound intensionalBound = new DefaultIntensionalBound();
	
	/**
	 * Returns a simplex on the the given Variables. 
	 * if ExtensionalRepresentation == true -> simplex is a extensional Bound
	 * else simplex is a extensional simplex
	 * @param variables : list of variables. one freeVAriable per expression
	 * @param model
	 * @return
	 */
	public static Bound simplex(List<Expression> variables, Model model, boolean ExtensionalRepresentation) {
		Bound result = ExtensionalRepresentation ? DefaultExtensionalBound.simplex(variables, model) : DefaultIntensionalBound.simplex(variables, model);
		return result;
	}
	
	/**
	 * Returns a simplex on the the given Variables. 
	 * if ExtensionalRepresentation == true -> simplex is a extensional Bound
	 * else simplex is a extensional simplex
	 * @param variables : list of variables. one freeVAriable per expression
	 * @param theory
	 * @param context
	 * @param ExtensionalRepresentation
	 * @return
	 */
	public static Bound simplex(List<Expression> variables, Theory theory, Context context, boolean ExtensionalRepresentation) {
		Bound result = ExtensionalRepresentation ? DefaultExtensionalBound.simplex(variables, theory, context) : DefaultIntensionalBound.simplex(variables, theory, context);
		return result;
	}
	
	/**
	 * Computes the product of each term of a list of bounds
	 * @param theory
	 * @param context
	 * @param listOfBounds
	 * @return bound resulting from the product of bounds
	 */
	public static Bound boundProduct(Theory theory, Context context, boolean isExtensional, Bound...listOfBounds) {
		
		if (isExtensional) {
			for (Bound bound : listOfBounds) {
				if (!bound.isExtensionalBound()) {
					return null;
				}
			}
			Bound result = DefaultExtensionalBound.boundProduct(theory, context, listOfBounds);
			return result;
		}
		if (!isExtensional) {
			for (Bound bound : listOfBounds) {
				if (!bound.isIntensionalBound()) {
					return null;
				}
			}
			Bound result = DefaultIntensionalBound.boundProduct(theory, context, listOfBounds);
			return result;
		}
		return null;
	}
	
	/**
	 * Eliminate factors not in Ext(C.Hull(bound))
	 * Only available for ExtensionalBounds
	 * @param bound
	 * @return 
	 */
	public static Bound updateExtremes(Bound bound, Theory theory, Context context) {
		if (!bound.isExtensionalBound()) {
			return null;
		}
		Bound result = DefaultExtensionalBound.updateExtremes(bound, theory, context);
		return result;
	}

	/**
	 * Make a one element bound: either a extensionalBound Singleton or a intensional Bound w/ indexes 
	 * @param head
	 * @param index
	 * @param condition
	 * @param isExtensional
	 * @return
	 */
	public static Bound makeSingleElementBound(Expression head, boolean isExtensional) {
		if (isExtensional) {
			return new DefaultExtensionalBound(head);
		}
		return new DefaultIntensionalBound(new ArrayList<Expression>() , head, Expressions.makeSymbol("true"));
	}
	
	/**
	 * Does not work for sets or bounds. Aims at normalizing a sing expression phi
	 * @param phi
	 * @param theory
	 * @param context
	 * @return
	 */
	public static Expression normalizeSingleExpression (Expression phi, Theory theory, Context context) {
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(phi, context);
		
		Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				phi,// head
				makeSymbol(true)// No Condition
				);
		
		Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
		Expression f =  apply("/", phi, sumOnPhi);
		
		Expression evaluation = theory.evaluate(f, context);
		return evaluation;
	}
	
//
//	/**
//	 * Assumes that each element of the bound is a factor with the same domain
//	 * Normalizes each factor of the bound. In latex notation: 
//	 * 			{\phi/sum_{var(\phi)}\phi : \phi in bound} 
//	 * @param bound
//	 * @param theory
//	 * @param context
//	 * @return  bound of normalized factors
//	 */
//	public static Bound normalize(Bound bound, Theory theory, Context context) {
//		Bound result;
//		if (bound.isExtensionalBound()) {
//			result = extensionalBound.normalize(bound, theory, context);
//		}
//		else if (bound.isIntensionalBound()) {
//			result = intensionalBound.normalize(bound, theory, context);
//		}
//		else{
//			result = null;
//		}
//		return result;
//	}
//	/**
//	 * given a set of variables "S" and a bound "bound", performs the following operation:
//	 * sum_S bound = {sum_S \phi : \phi in bound} 
//	 * 
//	 * @param variablesToBeSummedOut 
//	 * 		S in the example. 
//	 * 		Must be a Explicit UniSet. For example: {A,B,C} , where A, B and C are variables 
//	 * @param bound
//	 * 		bound in the example
//	 * @param context
//	 * @param theory
//	 * @return
//	 */
//	public static Bound summingBound(Expression variablesToBeSummedOut, Bound bound,
//			Context context, Theory theory) {
//		Bound result = null; 
//		if (bound.isExtensionalBound()) {
//			result = extensionalBound.summingBound(variablesToBeSummedOut, bound, context, theory);
//		}
//		else if (bound.isIntensionalBound()) {
//			result = intensionalBound.summingBound(variablesToBeSummedOut, bound, context, theory);
//		}								
//		return result;
//	}
//	
//	/**
//	 * given a set of variables "S" a factor \phi and a bound "bound", performs the following operation:
//	 * sum_S (\phi * bound) = {sum_S \phi \phi' : \phi' in bound} 
//	 * 
//	 * @param variablesToBeSummedOut 
//	 * 		S in the example. Must be a ExtensionalSet
//	 * @param phi
//	 * @param bound
//	 * 		bound in the example
//	 * @param context
//	 * @param theory
//	 * @return
//	 */
//	public static Bound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound,
//			Context context, Theory theory) {
//		Bound result = null; 
//		if (bound.isExtensionalBound()) {
//			result = extensionalBound.summingPhiTimesBound(variablesToBeSummedOut, phi, bound, context, theory);
//		}
//		else if (bound.isIntensionalBound()) {
//			result = intensionalBound.summingPhiTimesBound(variablesToBeSummedOut, phi, bound, context, theory);
//		}								
//		return result;		
//	}
//	
//	/**
//	 * Same as the other with the same name.
//	 * variablesToBeSummedOut is a list of Expressions. each expression must have one single free variable 
//	 * @param variablesToBeSummedOut
//	 * @param phi
//	 * @param bound
//	 * @param context
//	 * @param theory
//	 * @return
//	 */
//	public static Bound summingPhiTimesBound(ArrayList<Expression> variablesToBeSummedOut, Expression phi, Bound bound,
//			Context context, Theory theory) {
//		Expression setOfVariablesToBeSummedOut = new DefaultExtensionalUniSet(variablesToBeSummedOut);
//		Bound result = summingPhiTimesBound(setOfVariablesToBeSummedOut, phi, bound, context, theory);
//		return result;
//	}
//	
//	/**
//	 * Same as the other with the same name.
//	 * variablesToBeSummedOut is a list of Expressions. each expression must have one single free variable 
//	 * @param variablesToBeSummedOut
//	 * @param bound
//	 * @param context
//	 * @param theory
//	 * @return
//	 */
//	public static Bound summingBound(ArrayList<Expression> variablesToBeSummedOut, Bound bound,
//			Context context, Theory theory) {
//		Expression setOfVariablesToBeSummedOut = new DefaultExtensionalUniSet(variablesToBeSummedOut);
//		Bound result = summingBound(setOfVariablesToBeSummedOut, bound, context, theory);
//		return result;
//	}
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
