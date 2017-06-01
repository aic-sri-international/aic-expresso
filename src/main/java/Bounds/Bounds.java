package Bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
//import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet.getElements;
import static com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet.removeNonDestructively;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExtensionalSetInterface;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;

public class Bounds {
	// a bound is a set of expressions representing its extreme points
	
	static boolean debug = false;
	
	
	/**
	 * Assumes that each element of the bound is a factor with the same domain
	 * Normalizes each factor of the bound. In latex notation: 
	 * 			{\phi/sum_{var(\phi)}\phi : \phi in bound} 
	 * @param bound
	 * @param theory
	 * @param context
	 * @return  bound of normalized factors
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
		return result;
	}
	
	
	/**
	 * Computes the product of each term of a list of bounds
	 * @param theory
	 * @param context
	 * @param listOfBounds
	 * @return bound resulting from the product of bounds
	 */
	public static Expression boundProduct(Theory theory, Context context, Expression...listOfBounds){
		if(listOfBounds.length == 0){ 
			return null;
		}
		
		Expression result= boundProduct (0, theory, context, listOfBounds);
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
		
		DefaultExtensionalUniSet productBound = new DefaultExtensionalUniSet(elements);
		//Updating extreme points
		Expression result = updateExtremes(productBound,theory,context);
		return result;
	}
	
	/**
	 * apply a function to each term of a bound
	 * @param f 
	 * 			function to be applied to the factors
	 * @param variableName
	 * 			The variable in f to be replaced by phi (for each phi in b)
	 * @param b
	 * 			Bound
	 * @param theory
	 * @param context
	 * @return {f(\phi) : \phi \in b}
	 */ //TODO : use divide and conquer
	public static Expression applyFunctionToBound(Expression f, Expression variableName, Expression b, Theory theory, Context context){
		ExtensionalSetInterface bAsExtensionalSet = (ExtensionalSetInterface) b;
		int numberOfExtremes = bAsExtensionalSet.getArguments().size();
		ArrayList<Expression> elements = new ArrayList<>(numberOfExtremes);
		for(Expression phi : ExtensionalSet.getElements(bAsExtensionalSet)){
			Expression substitution = f.replaceAllOccurrences(variableName, phi, context);
			//debuging
			if (debug) println("evaluating: " + substitution);
			Expression evaluation = theory.evaluate(substitution, context); // problem in evaluation method...
			//debuging
			if (debug) println("result: " + evaluation);
			elements.add(evaluation);
		}
		DefaultExtensionalUniSet fOfb = new DefaultExtensionalUniSet(elements);
		//Updating extreme points
		Expression result = updateExtremes(fOfb,theory,context);		
		return result;
	}
	
	/**
	 * Eliminate factors not in Ext(C.Hull(B)) 
	 * @param B
	 * @return 
	 */
	private static Expression updateExtremes(Expression B,Theory theory, Context context){
		List<Expression> listOfB = getElements(B);
		ArrayList<Expression> elements = new ArrayList<>(listOfB.size());
		int indexPhi = 0;
		for(Expression phi : listOfB){
			if (isExtremePoint(phi,indexPhi,B,theory,context)){
				elements.add(phi);
			}
			indexPhi++;
		}
		DefaultExtensionalUniSet result = new DefaultExtensionalUniSet(elements);
		return result;
	}
	
	/**
	 * checks if \phi is a convex combination of the elements in bound
	 * @param phi
	 * 			factor
	 * @param bound
	 * @return
	 */
	public static boolean isExtremePoint(Expression phi,int indexPhi, Expression bound, Theory theory, Context context){
		//TODO
		
		Expression boundWithoutPhi = removeNonDestructively(bound, indexPhi);//caro pq recopia a lista toda
		List<Expression> listOfB = getElements(boundWithoutPhi);
		int n = listOfB.size();
		
		Expression[] c = new Expression[n];
		for(int i = 0;i<n;i++){
			c[i] = makeSymbol("c" + i);

			context = context.extendWithSymbols("c" + i,"Real");
		}
		
		String formula = "";
		//there exists ci in real
		for(int i = 0;i<n;i++){
			formula = formula + "there exists c" + i + " in Real : ";
		}
		// 0<=ci<=1
		for(int i = 0;i<n;i++){
			formula = formula + "c" + i + " >= 0 and c" + i + " <= 1 and ";
		}
		//sum over ci =1
		for(int i = 0;i<n;i++){
			formula = formula + "c" + i;
			if(i != n-1){
				formula = formula + " + ";
			}
			else{
				formula = formula + " = 1 and ";
			}
		}
		//sum of ci*phi1 = phi
		int i = 0;
		for(Expression phii : listOfB){
			formula = formula + phii + "*c" + i;
			if(i != n-1){
				formula = formula + " + ";
			}
			else{
				formula = formula + " = " + phi;
			}
			i++;
		}
		
		if(debug) println(formula);
		//Expression formulaOfExtremePoints = parse(formula);
		//Expression result = theory.evaluate(formulaOfExtremePoints, context);
		return true;
	}	
}
