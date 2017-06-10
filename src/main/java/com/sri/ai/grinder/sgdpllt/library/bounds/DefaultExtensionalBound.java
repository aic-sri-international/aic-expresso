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
import static com.sri.ai.util.Util.arrayList;
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
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;

public class DefaultExtensionalBound extends AbstractExtensionalBound{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	protected static boolean debug = false;
	
	public DefaultExtensionalBound(ArrayList<Expression> elementsDefinitions) {
		super(elementsDefinitions);
	}
	
	public DefaultExtensionalBound(Expression singleElement) {
		super(arrayList(singleElement));
	}

	public DefaultExtensionalBound() {
	}
	
	@Override
	public DefaultExtensionalBound simplex(List<Expression> Variables, Model model) {
		//TODO 	change function to receive theory + context instead of model (for coherence with the others)
		//		the change is quite straightforward, but lets wait for the others get adapted with this
		//		new class hierarchies designed for the bounds
		ArrayList<Expression> simplexList = new ArrayList<>();

		Expression one = makeSymbol("1");
		Expression zero= makeSymbol("0");
		
		for(Expression var : Variables){
			Type type = model.context.getTypeOfRegisteredSymbol(var);
			Iterator<Expression>  iteratorToValuesInType = type.iterator();
			
			for(Expression value : in(iteratorToValuesInType)){
				simplexList.add(apply(IF_THEN_ELSE, apply(EQUAL, var, value), one, zero));
			}
		}

		DefaultExtensionalBound result =  new DefaultExtensionalBound(simplexList);
		return result;
	}

	@Override
	public DefaultExtensionalBound normalize(Bound bound, Theory theory, Context context) {
		if(!bound.isExtensionalBound()){
			//TODO launch exception or something
			return null;
		}
		
		List<Expression> listOfBound = getElements(bound);
		if(listOfBound.size() == 0){
			return null;
		}
		
		int numberOfExtremes = listOfBound.size();
		
		ArrayList<Expression> elements = new ArrayList<>(numberOfExtremes);
		for(Expression element : listOfBound){
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(element, context);
			Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
					indices,
					element,//head
					makeSymbol(true)//No Condition
					);
			
			Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
			Expression f =  apply("/", element, sumOnPhi);
			
			Expression evaluation = theory.evaluate(f, context);
			elements.add(evaluation);
		}
		DefaultExtensionalBound fOfBound = new DefaultExtensionalBound(elements);
		//Updating extreme points
		DefaultExtensionalBound result = updateExtremes(fOfBound,theory,context);		
		return result;
	}

	@Override
	public DefaultExtensionalBound boundProduct(Theory theory, Context context, Expression... listOfBounds) {		
		if(listOfBounds.length == 0){
			DefaultExtensionalBound singletonWithNumberOne = new DefaultExtensionalBound(parse("1"));
			return singletonWithNumberOne;
		}
		
		ArrayList<NullaryFunction<Iterator<Expression>>> iteratorForBoundList = 
				mapIntoArrayList(listOfBounds, bound -> () -> getElements(bound).iterator());
		
		Iterator<ArrayList<Expression>> cartesianProduct = new CartesianProductIterator<Expression>(iteratorForBoundList);
		
		if(!cartesianProduct.hasNext()){
			DefaultExtensionalBound singletonWithNumberOne = new DefaultExtensionalBound(parse("1"));
			println("One of the bounds on the list is { }, which is an error");
			return singletonWithNumberOne;
		}
		
		ArrayList<Expression> resultList = new ArrayList<>();
		for (ArrayList<Expression> element : in(cartesianProduct)){
			Expression product = apply("*",element);
			Expression evaluation = theory.evaluate(product,context);
			resultList.add(evaluation);
		}
		
		DefaultExtensionalBound result =  new DefaultExtensionalBound(resultList);
		
		// Updating extreme points
		result = updateExtremes(result, theory, context);
		
		return result;		
	}

	@Override
	public DefaultExtensionalBound summingBound(Expression variablesToBeSummedOut, Bound bound, Context context, Theory theory) {

		List<Expression> listOfBound = getElements(bound);
		ArrayList<Expression> BoundSummedOut = new ArrayList<>(listOfBound.size());
		
		for(Expression phi : listOfBound){
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variablesToBeSummedOut, context);
			
			Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
					indices,
					phi,//head
					makeSymbol(true)//No Condition
					);
			
			Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
			Expression evaluation = theory.evaluate(sumOnPhi, context);
			BoundSummedOut.add(evaluation);
		}
		
	DefaultExtensionalBound SetOfBoundSummedOut = new DefaultExtensionalBound(BoundSummedOut);
	//Updating extreme points
	DefaultExtensionalBound result = updateExtremes(SetOfBoundSummedOut,theory,context);		
	result = normalize(result, theory, context);
	return result;
	}

	@Override
	public DefaultExtensionalBound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound,
			Context context, Theory theory) {
		Expression x = createSymbol("x");
		Expression f = apply(TIMES, phi, x);		
		bound = normalize(bound, theory, context);
		DefaultExtensionalBound phiTimesBound = applyFunctionToBound(f, x, bound, theory, context);	
		
		DefaultExtensionalBound result = summingBound(variablesToBeSummedOut, phiTimesBound, context, theory);
		
		return result;
	}
	

	@Override
	public DefaultExtensionalBound applyFunctionToBound(Expression f, Expression variableName, Bound bound, Theory theory, Context context){
		ExtensionalSet bAsExtensionalSet = (ExtensionalSet) bound;
		int numberOfExtremes = bAsExtensionalSet.getArguments().size();
		ArrayList<Expression> elements = new ArrayList<>(numberOfExtremes);
		for(Expression phi : ExtensionalSets.getElements(bAsExtensionalSet)){
			Expression substitution = f.replaceAllOccurrences(variableName, phi, context);
			if (debug) println("evaluating: " + substitution);
			Expression evaluation = theory.evaluate(substitution, context);
			if (debug) println("result: " + evaluation);
			elements.add(evaluation);
		}
		DefaultExtensionalBound fOfb = new DefaultExtensionalBound(elements);
		//Updating extreme points
		DefaultExtensionalBound result = updateExtremes(fOfb,theory,context);		
		return result;
	}

	
	//-------------------------

	 /** Eliminate factors not in Ext(C.Hull(B)) 
	 * @param B
	 * @return 
	 */
	public static DefaultExtensionalBound updateExtremes(Bound B,Theory theory, Context context){
		List<Expression> listOfB = getElements(B);
		ArrayList<Expression> elements = new ArrayList<>(listOfB.size());
		int indexPhi = 0;
		for(Expression phi : listOfB){
			if (isExtremePoint(phi,indexPhi,B,theory,context)){
				elements.add(phi);
			}
			indexPhi++;
		}
		DefaultExtensionalBound result = new DefaultExtensionalBound(elements);
		return result;
	}
	
	/**
	 * Checks if \phi is a convex combination of the elements in bound
	 * @param phi
	 * 			factor
	 * @param bound
	 * @return
	 */
	public static boolean isExtremePoint(Expression phi,int indexPhi, Bound bound, Theory theory, Context context){
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
		//if (true) println(result);
		return true;
	}
}
