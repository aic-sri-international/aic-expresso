package Bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
//import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExtensionalSetInterface;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
//import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
//import com.sri.ai.util.Util;


public class Bounds {
	// a bound is a set of expressions representing its extreme points
	
	
	/**
	 * 
	 * 
	 * Assumes that each element of the bound is a factor with the same domain 
	 */
	public static Expression normalize(Expression bound, Theory theory, Context context){
		List<Expression> listOfBound = ExtensionalSet.getElements(bound);
		if(listOfBound.size() == 0){
			return null;
		}
		
		Expression phi = makeSymbol("phi");
	
		Expression phi1 = listOfBound.get(0);
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(phi1, context);
		Expression noCondition = makeSymbol(true);
		Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				phi,//head
				noCondition);
		
		Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
		Expression f =  apply("/", phi, sumOnPhi);
		Expression result = applyFunctionToBound(f, phi, bound, theory, context);
		//update limits (eliminate redundancies)
		return result;
	}
	
	public static Expression boundProduct(Theory theory, Context context, Expression...listOfBounds){
		if(listOfBounds.length == 0){ 
			return null;
		}
		
		Expression result = boundProduct (0, theory, context, listOfBounds);
		return result;
	}

	private static Expression boundProduct(int i,  Theory theory, Context context, Expression...listOfBounds){
		if(listOfBounds.length - 1 == i){
			return listOfBounds[i];
		}
		
		Expression productOfOthers = boundProduct(i + 1, theory, context, listOfBounds);
		Expression b = listOfBounds[i];

		List<Expression> listOfb = ExtensionalSet.getElements(b);
		List<Expression> listOfProductOfOthers = ExtensionalSet.getElements(productOfOthers);
		
		ArrayList<Expression> elements = new ArrayList<>(listOfb.size()*listOfProductOfOthers.size());
		
		for (Expression phi1 : listOfb){
			for (Expression phi2 : listOfProductOfOthers){
				Expression product = apply("*",phi1,phi2);
				Expression evaluation = theory.evaluate(product,context);
				elements.add(evaluation);
			}
		}
		//update extremes
		DefaultExtensionalUniSet result = new DefaultExtensionalUniSet(elements);
		return result;
	}
	
	public static Expression applyFunctionToBound(Expression f, Expression variableName, Expression b, Theory theory, Context context){
		ExtensionalSetInterface bAsExtensionalSet = (ExtensionalSetInterface) b;
		int numberOfExtremes = bAsExtensionalSet.getArguments().size();
		ArrayList<Expression> elements = new ArrayList<>(numberOfExtremes);
		for(Expression phi : ExtensionalSet.getElements(bAsExtensionalSet)){
			Expression substitution = f.replaceAllOccurrences(variableName, phi, context);
			println("evaluating: " + substitution);
			Expression evaluation = theory.evaluate(substitution, context);
			println("result: " + evaluation);
			elements.add(evaluation);
		}
		DefaultExtensionalUniSet result = new DefaultExtensionalUniSet(elements);
		return result;
	}
	
	/*private static Expression updateExtremes(Expression B){
		List<Expression> listOfB = ExtensionalSet.getElements(B);
		ArrayList<Expression> elements = new ArrayList<>(listOfB.size());
		for(Expression phi : listOfB){
			
		}
		
	}
	 */
}
