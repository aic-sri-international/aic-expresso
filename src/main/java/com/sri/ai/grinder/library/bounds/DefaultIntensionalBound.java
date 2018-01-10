package com.sri.ai.grinder.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.library.FunctorConstants.AND;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.library.FunctorConstants.IN;
import static com.sri.ai.grinder.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
// import static com.sri.ai.grinder.library.FunctorConstants.PRODUCT;





import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.anytime.Model;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;

public class DefaultIntensionalBound extends AbstractIntensionalBound{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Empty Bound
	 */
	public DefaultIntensionalBound() {
		super(new ExtensionalIndexExpressionsSet(new ArrayList<>()),makeSymbol("1"),makeSymbol(true));
	}

	public DefaultIntensionalBound(IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		super(indexExpressions, head, condition);
		}

	public DefaultIntensionalBound(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		this(new ExtensionalIndexExpressionsSet(indexExpressionsList), head, condition);
	}

	static public DefaultIntensionalBound simplex(List<Expression> Variables, Model model) {
		DefaultIntensionalBound result = simplex(Variables, model.theory,model.context);
		return result;
	}
	
	static public DefaultIntensionalBound simplex(List<Expression> Variables, Theory theory, Context context) {
		if (Variables.size() == 0) {
			return new DefaultIntensionalBound();
		}
		Expression one = makeSymbol("1");
		Expression zero= makeSymbol("0");
		List<Expression> indexExpressionsList = new ArrayList<>(Variables.size());
		
		// ExtensionalIndexExpressionsSet indexExpressions = GrinderUtil.makeIndexExpressionsForIndicesInListAndTypesInRegistry(Variables, context);
				
		Expression head = one;
		for (Expression var : Variables) {
			Expression index = Expressions.primedUntilUnique(var, var, context);
			Expression type = context.getTypeExpressionOfRegisteredSymbol(var);
			Expression indexExpression = IndexExpressions.makeIndexExpression(index, type);
			
			indexExpressionsList.add(indexExpression);
			
			head = apply(
					IF_THEN_ELSE,apply(EQUAL,var,index),
						head,
						zero);
		}
		
		Expression noCondition = makeSymbol(true);
		
		ExtensionalIndexExpressionsSet indexExpressions = new ExtensionalIndexExpressionsSet(indexExpressionsList);
		DefaultIntensionalBound simplex = new DefaultIntensionalBound(indexExpressions, head, noCondition);
		return simplex;
	}

	@Override
	public DefaultIntensionalBound normalize(Theory theory, Context context) {
		DefaultIntensionalBound result = normalize(this, theory, context);
		return result;
	}
	
	private DefaultIntensionalBound normalize(Bound bound, Theory theory, Context context) 
	{
		if (!bound.isIntensionalBound()) {
			return null;
		}
		
		DefaultIntensionalBound intensionalBound = (DefaultIntensionalBound) bound;
		
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalBound.getIndexExpressions();
		Expression Head                      = intensionalBound.getHead();
		Expression condition                 = intensionalBound.getCondition();
		
		Set<Expression> hashSetOfindexVariables = Util.set();
		
		ExtensionalIndexExpressionsSet freeVariablesOfTheHead = (ExtensionalIndexExpressionsSet) getIndexExpressionsOfFreeVariablesIn(Head, context);
		
		for (Expression indexExpression: indexExpressions.getList()) {
			Symbol index = (Symbol) indexExpression.get(0);
			Expression type = indexExpression.get(1);
			context = context.extendWithSymbolsAndTypes(index,type);
			
			hashSetOfindexVariables.add(index);
		}
		
		ArrayList<Expression> variablesToSumOutList = new ArrayList<>();
		for (Expression indexExpression: freeVariablesOfTheHead.getList()) {
			Expression index = indexExpression.getFunctorOrSymbol();// Gambiarra!
			if (!hashSetOfindexVariables.contains(index)) {
				variablesToSumOutList.add(indexExpression);
			}
		}
		
		ExtensionalIndexExpressionsSet variablesToSumOut = new ExtensionalIndexExpressionsSet(variablesToSumOutList);
		
		Expression setOfInstantiationsOfTheHead = IntensionalSet.makeMultiSet(
				variablesToSumOut,
				Head,// head
				makeSymbol(true)// No Condition
				);
		
		Expression sumOnPhi = apply(SUM, setOfInstantiationsOfTheHead);
		
		// sumOnPhi = theory.evaluate(sumOnPhi, context);
		Expression normalizedHead =  apply("/", Head, sumOnPhi);
		
		Expression evaluation = theory.evaluate(normalizedHead, context);
		
		DefaultIntensionalBound normalizedIntensionalSet = 
				new DefaultIntensionalBound(indexExpressions, evaluation, condition);
		return normalizedIntensionalSet;		
	}

	public static DefaultIntensionalBound boundProduct(Theory theory, Context context, Bound... listOfBounds) {
		if (listOfBounds.length == 0) {
			DefaultIntensionalBound result = new DefaultIntensionalBound();
			return result;
		}
		
		Set<Expression> alreadyDefined = Util.set();
		alreadyDefined.addAll(context.getSymbols());
		Predicate<Expression> isAlreadyDefined = e -> alreadyDefined.contains(e);
		
		ArrayList<Expression> productIndexExpressionList = new ArrayList<>();
		Object[] productHeadArray = new Expression[listOfBounds.length]; 
		Object[] productConditionArray = new Expression[listOfBounds.length];
		
		int k = 0;
		for (Bound bound : Arrays.asList(listOfBounds)) {
			if (!bound.isIntensionalBound()) {
				return null;
			}
			
			DefaultIntensionalBound intensionalBound = (DefaultIntensionalBound) bound;
			
			ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalBound.getIndexExpressions();
			Expression Head                			      	= intensionalBound.getHead();
			Expression condition                 			= intensionalBound.getCondition();
			
			ArrayList<Expression> newIndexExpressionsList = new ArrayList<>(indexExpressions.getList());
			
			for (int i = 0; i != newIndexExpressionsList.size(); i++) {
				Expression indexExpression = newIndexExpressionsList.get(i);
				Symbol index = (Symbol) indexExpression.get(0);
				Expression type = indexExpression.get(1);
				PairOf<Expression> newIndexAndNewExpressionInScope = Expressions.standardizeApart(index, isAlreadyDefined, Head);
				
				Expression newIndex = newIndexAndNewExpressionInScope.first;
				Head = newIndexAndNewExpressionInScope.second;
				Expression newIndexExpression = apply(IN, newIndex, type); // type should not contain the index
				context = context.extendWithSymbolsAndTypes(newIndex,type);
				newIndexExpressionsList.set(i, newIndexExpression);
				alreadyDefined.add(newIndex);
				for (int j = i + 1; j != newIndexExpressionsList.size(); j++) {
					Expression anotherIndexExpression = newIndexExpressionsList.get(j);
					Expression anotherIndex = anotherIndexExpression.get(0);
					Expression anotherType = anotherIndexExpression.get(1);
					Expression newAnotherType = anotherType.replaceSymbol(index, newIndex, context);
					Expression newAnotherIndexExpression = apply(IN, anotherIndex, newAnotherType); // anotherIndex is a symbols and does not contain index
					newIndexExpressionsList.set(j, newAnotherIndexExpression);
				}				
			}
			productIndexExpressionList.addAll(newIndexExpressionsList);

			productHeadArray[k] = Head;
			productConditionArray[k] = condition;
			k++;
		}
		
		Expression	productCondition = apply(AND, productConditionArray);
		productCondition =theory.evaluate(productCondition, context);
		
		Expression productHead = apply(TIMES,productHeadArray);
		productHead = theory.evaluate(productHead, context);
		
		DefaultIntensionalBound result = new DefaultIntensionalBound(productIndexExpressionList,productHead,productCondition);
		return result;
	}

	@Override
	public DefaultIntensionalBound summingBound(Expression variablesToBeSummedOut, Context context, Theory theory) {
		DefaultIntensionalBound result = summingBound(variablesToBeSummedOut, this, context, theory);
		return result;
	}
	
	private DefaultIntensionalBound summingBound(Expression variablesToBeSummedOut, Bound bound, Context context, Theory theory) {
		if (!bound.isIntensionalBound()) {
			return null;
		}
		DefaultIntensionalBound intensionalBound = (DefaultIntensionalBound) bound;
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalBound.getIndexExpressions();
		for (Expression indexExpression : indexExpressions.getList()) {
			Expression index = indexExpression.get(0);
			Expression type = indexExpression.get(1);
			context = context.extendWithSymbolsAndTypes(index,type);
		}
		
		Expression x = makeSymbol("variableX");
		
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variablesToBeSummedOut, context);
		
		Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				x,// head
				makeSymbol(true)// No Condition
				);
		Expression f = apply(SUM, setOfFactorInstantiations);
		
		DefaultIntensionalBound result = applyFunctionToBound(f, x, bound, theory, context);
		result = normalize(result, theory, context);
		return result;		
	}

	@Override
	public DefaultIntensionalBound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Context context,
			Theory theory) {
		DefaultIntensionalBound result = summingPhiTimesBound(variablesToBeSummedOut, phi, this, context, theory);
		return result;
	}
	
	private DefaultIntensionalBound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound, Context context,
			Theory theory) {
		if (!bound.isIntensionalBound()) {
			return null;
		}
		
		DefaultIntensionalBound intensionalBound = (DefaultIntensionalBound) bound;
		ExtensionalIndexExpressionsSet indexExpressions = (ExtensionalIndexExpressionsSet) intensionalBound.getIndexExpressions();
		for (Expression indexExpression : indexExpressions.getList()) {
			Expression index = indexExpression.get(0);
			Expression type = indexExpression.get(1);
			context = context.extendWithSymbolsAndTypes(index,type);
		}
		
		
		Expression x = makeSymbol("l");
		Expression f = apply(TIMES, x,phi);
		
		DefaultIntensionalBound fOfBound = applyFunctionToBound(f, x, bound, theory, context);
		
		DefaultIntensionalBound result = summingBound(variablesToBeSummedOut, fOfBound, context, theory);
		return result;				
	}
	
	@Override
	public Bound summingBound(ArrayList<Expression> variablesToBeSummedOut, Context context, Theory theory) {
		Expression varSet  = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		return summingBound(varSet, context, theory); 
	}

	@Override
	public Bound summingPhiTimesBound(ArrayList<Expression> variablesToBeSummedOut, Expression phi, Context context,
			Theory theory) {
		Expression varSet  = new DefaultExtensionalUniSet(variablesToBeSummedOut);
		return summingPhiTimesBound(varSet, phi, context, theory); 
	}

	protected DefaultIntensionalBound applyFunctionToBound(Expression f, Expression variableName, Bound bound, Theory theory, Context context) {
		if (!bound.isIntensionalBound()) {
			return null;
		}
		
		IntensionalSet intensionalBound = (IntensionalSet) bound;
		
		IndexExpressionsSet indexExpressions = intensionalBound.getIndexExpressions();
		Expression Head                      = intensionalBound.getHead();
		Expression condition                 = intensionalBound.getCondition();
		
		Expression fOfHead = f.replaceAllOccurrences(variableName, Head, context);
		Expression evaluation = theory.evaluate(fOfHead, context);
		
		DefaultIntensionalBound result = new DefaultIntensionalBound(indexExpressions, evaluation, condition);
		return result;
	}


}
