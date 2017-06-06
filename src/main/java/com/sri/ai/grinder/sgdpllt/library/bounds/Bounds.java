package com.sri.ai.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.core.DefaultSymbol.createSymbol;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.AND;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets.getElements;
import static com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets.removeNonDestructively;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExtensionalSet;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;


public class Bounds {
	// a bound is a set of expressions representing its extreme points

	static boolean debug = false;
	
	public static Expression simplex(List<Expression> Variables, Model model){
		ArrayList<Expression> simplexList = new ArrayList<>();

		Expression one = makeSymbol("1");
		Expression zero= makeSymbol("0");
		
		for(Expression var : Variables){
			Expression values = model.getValues(var); 
			List<Expression> listOfValues = getElements(values);
			for (Expression value : listOfValues){
				simplexList.add(apply(IF_THEN_ELSE, apply(EQUAL, var, value), one, zero));
			}
		}

		Expression result =  new DefaultExtensionalUniSet(simplexList);
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
	public static Expression normalize(Expression bound, Theory theory, Context context){
		List<Expression> listOfBound = ExtensionalSets.getElements(bound);
		if(listOfBound.size() == 0){
			return null;
		}
		
		Expression phi = makeSymbol("phi");
	
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(bound, context);
		
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
		
		ArrayList<NullaryFunction<Iterator<Expression>>> iteratorForBoundList = 
				mapIntoArrayList(listOfBounds, bound -> () -> getElements(bound).iterator());
		
		Iterator<ArrayList<Expression>> cartesianProduct = new CartesianProductIterator<Expression>(iteratorForBoundList);
		
		ArrayList<Expression> resultList = new ArrayList<>();
		for (ArrayList<Expression> element : in(cartesianProduct)){
			Expression product = apply("*",element);
			Expression evaluation = theory.evaluate(product,context);
			resultList.add(evaluation);
		}
		
		Expression result =  new DefaultExtensionalUniSet(resultList);
		
		// Updating extreme points
		result = updateExtremes(result, theory, context);
		
		return result;		
	}
	
	/**
	 * apply a function (f) to each term of a bound (b) 
	 * Example: if we have a function f(x) and a bound b = {a,b,c} and we want to compute 
	 * 			f(b) = {f(a), f(b), f(c)}
	 * 			Then it suffices to pass as arguments : 	(1) f as function 
	 * 													(2) x as variableName
	 * 													(3) b as bound
	 * @param f 
	 * 			function to be applied to the factors
	 * @param variableName
	 * 			The variable in f to be replaced by phi (for each phi in b). 
	 * 			If if is a function of the variable v, VariableName is v
	 * @param b
	 * 			Bound
	 * @param theory
	 * @param context
	 * @return {f(\phi) : \phi \in b}
	 */
	public static Expression applyFunctionToBound(Expression f, Expression variableName, Expression b, Theory theory, Context context){
		ExtensionalSet bAsExtensionalSet = (ExtensionalSet) b;
		int numberOfExtremes = bAsExtensionalSet.getArguments().size();
		ArrayList<Expression> elements = new ArrayList<>(numberOfExtremes);
		for(Expression phi : ExtensionalSets.getElements(bAsExtensionalSet)){
			Expression substitution = f.replaceAllOccurrences(variableName, phi, context);
			if (debug) println("evaluating: " + substitution);
			Expression evaluation = theory.evaluate(substitution, context);
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
	 * Checks if \phi is a convex combination of the elements in bound
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
			context = context.extendWithSymbolsAndTypes("c" + i,"Real");
		}
		
		// 0<=ci<=1
		ArrayList<Expression> listOfC = new ArrayList<>(listOfB.size());
		for(int i = 0;i<n;i++){
			Expression cibetwen0And1 = 
					apply(AND,apply(GREATER_THAN_OR_EQUAL_TO,1,c[i]),
							  apply(GREATER_THAN_OR_EQUAL_TO,c[i],0)
						  );
			listOfC.add(cibetwen0And1);
		}
		Expression allcibetwen0And1 = apply(AND, listOfC);
		
		//sum over ci =1
		listOfC = new ArrayList<>(Arrays.asList(c));
		Expression sumOverCiEqualsOne = apply(EQUAL,1,apply(PLUS,listOfC));

		//sum of ci*phi1 = phi
		ArrayList<Expression> prodciphii = new ArrayList<>(listOfB.size());
		int i = 0;
		for(Expression phii : listOfB){
			prodciphii.add(apply(TIMES,phii,c[i]));
			i++;
		}
		Expression convexSum = apply(EQUAL,phi,apply(PLUS, prodciphii));
		
		ArrayList<Expression> listOfCiInReal = new ArrayList<>(listOfB.size());
		for(i = 0; i <n; i++){
			listOfCiInReal.add(apply(IN,c[i],"Real"));
		}
		IndexExpressionsSet thereExistsCiInReal = new ExtensionalIndexExpressionsSet(listOfCiInReal);
		
		Expression body = apply(AND, allcibetwen0And1, sumOverCiEqualsOne, convexSum);
		Expression isExtreme = new DefaultExistentiallyQuantifiedFormula(thereExistsCiInReal,body);
		
		if (debug) println(isExtreme);
		//Expression result = theory.evaluate(isExtreme, context);
		return true;
	}
	
	public static Expression summingBound(Expression variablesToBeSummedOut, Expression bound,
			Context context, Theory theory){
		Expression x = parse("x");
		
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variablesToBeSummedOut, context);
		
		Expression noCondition = makeSymbol(true);
		Expression summingoutSet = IntensionalSet.makeMultiSet(
				indices,
				x,//head
				noCondition);
		
		Expression f = apply(SUM,summingoutSet);
		Expression result = applyFunctionToBound(f, x, bound, theory, context);
		return result;
	}

	public static Expression summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Expression bound,
			Context context, Theory theory){
		Expression x = createSymbol("x");
		Expression f = apply(TIMES, phi, x);		
		
		Expression phiTimesBound = applyFunctionToBound(f, x, bound, theory, context);
		
		Expression result = summingBound(variablesToBeSummedOut, phiTimesBound, context, theory);
		return result;		
	}
	
	public static void main(String[] args) {
			
	Theory theory = new CompoundTheory(
			new EqualityTheory(false, true),
			new DifferenceArithmeticTheory(false, false),
			new LinearRealArithmeticTheory(false, false),
			new TupleTheory(),
			new PropositionalTheory());
	
	Context context = new TrueContext(theory);
	context = context.extendWithSymbolsAndTypes("X","Boolean");
	context = context.extendWithSymbolsAndTypes("Y","Boolean");
	context = context.extendWithSymbolsAndTypes("A","Boolean");
	context = context.extendWithSymbolsAndTypes("B","Boolean");

	//Set of functions
	Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
	Expression phi2 = parse("if X = true then if Y = true then 4 else 5 else 6");
	Expression phi3 = parse("if A = true then 7 else if B = true then 8 else 9");
	Expression phi4 = parse("if X = true then 10 else if Y = true then 11 else 12");
	Expression setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3, phi4);
	
	println(Bounds.normalize(setOfFactors, theory, context));
	
	println(Bounds.normalize(Bounds.summingBound(parse("{A,B,X}"), setOfFactors, context, theory) , theory, context));
	
}
	
}
