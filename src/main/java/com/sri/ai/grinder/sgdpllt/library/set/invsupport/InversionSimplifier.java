/*
 * Copyright (c) 2017, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Disequality;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.Implication;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class InversionSimplifier implements Simplifier {
// TODO - 1. Add support for summations and products with more than 1 index.
	
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (isSummationIndexedByFunctionOfQuantifiers(expression, context)) {
			// NOTE: at this point we know we have a summation indexed by a function
			Expression summationIndexedByFunction = expression;
			
			Pair<Expression, FunctionType> indexAndFunctionType = getIndexAndFunctionType(summationIndexedByFunction, context);
			Expression summationIndexFunctionName               = indexAndFunctionType.first;
			FunctionType summationIndexFunctionType             = indexAndFunctionType.second;
			
			List<Expression> originalQuantifierOrder  = new ArrayList<>();
			collectQuantifiers(summationIndexedByFunction, originalQuantifierOrder);
			List<Expression> inversionQuantifierOrder = new ArrayList<>();
			if (isInversionPossible(summationIndexedByFunction, summationIndexFunctionName, summationIndexFunctionType, originalQuantifierOrder, inversionQuantifierOrder, context)) {				
				result = applyInversion(summationIndexedByFunction, summationIndexFunctionName, summationIndexFunctionType, originalQuantifierOrder, inversionQuantifierOrder, context);
			}
		}
		
		return result;
	}
	
	private static boolean isInversionPossible(Expression summationIndexedByFunction, Expression summationIndexFunctionName, FunctionType summationIndexFunctionType, 
			List<Expression> originalQuantifierOrder, List<Expression> inversionQuantifierOrder, Context context) {		
		boolean result = false;
				
		List<Expression> productCandidatesToInvert = new ArrayList<>();
		int i = 1; // Start at 1 as 0 is the quantifier.
		// The only candidates considered are directly nested products underneath the summation
		// any other type of quantier (e.g an inner summation) is not considered (as can be 
		// handled by an exhaustive recursive rewriter using this simplifier).
		while (i < originalQuantifierOrder.size() && originalQuantifierOrder.get(i).hasFunctor(FunctorConstants.PRODUCT)) {
			productCandidatesToInvert.add(originalQuantifierOrder.get(i));
			i++;
		}			
		
		// Incrementally build up a list of products that can be moved before the summation
		List<Expression> productsThatCanBeInverted = new ArrayList<>();
		for (Expression productCandidate : productCandidatesToInvert) {
			List<Expression> productsBeforeToTest = new ArrayList<>(productsThatCanBeInverted);
			productsBeforeToTest.add(productCandidate);
			updateInversionOrder(originalQuantifierOrder, productsBeforeToTest, inversionQuantifierOrder);
			
			if (isInvertible(summationIndexedByFunction, summationIndexFunctionName, summationIndexFunctionType, originalQuantifierOrder, inversionQuantifierOrder, context)) {
				productsThatCanBeInverted.add(productCandidate);
			}
		}
		
		if (productsThatCanBeInverted.size() > 0) {
			updateInversionOrder(originalQuantifierOrder, productsThatCanBeInverted, inversionQuantifierOrder);
			result = true;
		}
		
		return result;
	}
	
	private static void updateInversionOrder(List<Expression> originalQuantifierOrder, List<Expression> productsBefore, List<Expression> inversionQuantifierOrder) {
		inversionQuantifierOrder.clear();
		inversionQuantifierOrder.addAll(productsBefore);
		for (Expression quantifier : originalQuantifierOrder) {
			if (!productsBefore.contains(quantifier)) {
				inversionQuantifierOrder.add(quantifier);
			}
		}
	}
	
	private static boolean isInvertible(Expression summationIndexedByFunction, Expression summationIndexFunctionName, FunctionType summationIndexFunctionType, 
			List<Expression> originalQuantifierOrder, List<Expression> inversionQuantifierOrder, Context context) {
		boolean result = false;
		
		int indexOfSummationIndexedByFunction = inversionQuantifierOrder.indexOf(summationIndexedByFunction);	
	
		// NOTE: we will use the full nested context in order to derive primedUntilUnique variables to ensure
		// we have globally unique variables in the expressions (simplifies reading/understanding).
		Expression lastQuantifierHead = getHead(originalQuantifierOrder.get(originalQuantifierOrder.size()-1));	
		Context lastQuantifierHeadContext = context;
		for (Expression quantifier : originalQuantifierOrder) {
			lastQuantifierHeadContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(getIndexExpressions(quantifier), lastQuantifierHeadContext);
		}

		// Construct E correctly based on quantifiers after summation.
		Expression E = lastQuantifierHead;
		List<Expression> quantifiersAfter = inversionQuantifierOrder.subList(indexOfSummationIndexedByFunction+1, inversionQuantifierOrder.size());
		for (int i = quantifiersAfter.size()-1; i >= 0; i--) {
			E = quantifyE(E, quantifiersAfter.get(i));			
		}

		// Now compute ocfE
		Expression ocfE = SetOfArgumentTuplesForFunctionOccurringInExpression.compute(summationIndexFunctionName, summationIndexFunctionType, E);
		
		// NOTE: only products will bubble up before the summation.
		List<Expression> productsBefore = inversionQuantifierOrder.subList(0, indexOfSummationIndexedByFunction);
		// Create the two sets of replacement quantifiers before summation indexed by function indices
		// to ensure we have disjoint applications.
		List<Expression> productsBeforeIndices = new ArrayList<>();
		for (Expression productBefore : productsBefore) {
			productsBeforeIndices.add(getIndexAndType(productBefore).first);
		}
		List<Expression> allBeforeIndices = new ArrayList<>(productsBeforeIndices);
		
		List<Expression> productsBeforeIndicesPrime  = new ArrayList<>();
		List<Expression> productsBeforeIndices2Prime = new ArrayList<>();
		for (int i = 0; i < productsBeforeIndices.size(); i++) {
			Expression productBeforeIndex = productsBeforeIndices.get(i);
			
			Expression allIndicesTuple = Expressions.makeTuple(allBeforeIndices);
			Expression productBeforeIndexPrime = Expressions.primedUntilUnique(productBeforeIndex, allIndicesTuple, lastQuantifierHeadContext);				
			productsBeforeIndicesPrime.add(productBeforeIndexPrime);
			allBeforeIndices.add(productBeforeIndexPrime);
			
			allIndicesTuple = Expressions.makeTuple(allBeforeIndices);
			Expression productBeforeIndex2Prime = Expressions.primedUntilUnique(productBeforeIndex, allIndicesTuple, lastQuantifierHeadContext);				
			productsBeforeIndices2Prime.add(productBeforeIndex2Prime);
			allBeforeIndices.add(productBeforeIndex2Prime);
		}

		// Create the antecendant part of implication for condition testing inversion
		// i.e.
		//     C_1[x_1/x'_1] and ... C_k[x_k/x'_k]
		// and C_1[x_1/x''_1] and ... C_k[x_k/x''_k]
		// and (x'_1,...,x'_k) != (x''_1,...,x''_k)
		List<Expression> conjunctsPrime  = new ArrayList<>();
		List<Expression> conjuncts2Prime = new ArrayList<>();
		for (int i = 0; i < productsBefore.size(); i++) {
			Expression quantifierBefore = productsBefore.get(i);
			Expression condition        = getCondition(quantifierBefore);
			
			// C_n[x_n/x'_n]
			Expression conjunctPrime = replaceAll(condition, productsBeforeIndices, productsBeforeIndicesPrime, lastQuantifierHeadContext);
			conjunctsPrime.add(conjunctPrime);
			
			// C_n[x_n/x''_n]
			Expression conjunct2Prime = replaceAll(condition, productsBeforeIndices, productsBeforeIndices2Prime, lastQuantifierHeadContext);
			conjuncts2Prime.add(conjunct2Prime);
		}
	
		// (x'_1,...,x'_k) != (x''_1,...,x''_k)
		Expression primesNotEqual = Disequality.make(
				Expressions.makeTuple(productsBeforeIndicesPrime),
				Expressions.makeTuple(productsBeforeIndices2Prime));
		
		List<Expression> allConjuncts = new ArrayList<>();
		allConjuncts.addAll(conjunctsPrime);
		allConjuncts.addAll(conjuncts2Prime);
		allConjuncts.add(primesNotEqual);
		
		Expression conjunct = And.make(allConjuncts);
		
		// Create the consequent part of implication for condition testing inversion
		// i.e.:
		// (oc_f[E][x_1/x'_1,....,x_k/x'_k]
		//       intersection
		//  oc_f[E][x_1/x''_1,....,x_k/x''_k])
		// = {}
		Expression ocfEPrime  = replaceAll(ocfE, productsBeforeIndices, productsBeforeIndicesPrime, lastQuantifierHeadContext);
		Expression ocfE2Prime = replaceAll(ocfE, productsBeforeIndices, productsBeforeIndices2Prime, lastQuantifierHeadContext);
		
		Expression intersection = Sets.makeIntersection(ocfEPrime, ocfE2Prime);
		
		Expression equality = Equality.make(intersection, Sets.EMPTY_SET);
		
		Expression implication = Implication.make(conjunct, equality);

		// Collect the index expressions for the universal quantifiers:
		// i.e.
		// for all x'_1 el. T_1 ... for all x'_k el. T_k for all x''_1 el. T_1 ... for all x''_k el. T_k
		List<Expression> productsBeforeIndexExpressionSetsPrime  = new ArrayList<>();
		List<Expression> productsBeforeIndexExpressionSets2Prime = new ArrayList<>();
		for (int i = 0; i < productsBefore.size(); i++) {
			Expression productBefore          = productsBefore.get(i);
			Expression productBeforeIndexType = getIndexAndType(productBefore).second;
			
			Expression indexExpressionPrime = IndexExpressions.makeIndexExpression(productsBeforeIndicesPrime.get(i), productBeforeIndexType);
			productsBeforeIndexExpressionSetsPrime.add(indexExpressionPrime);
			
			Expression indexExpression2Prime = IndexExpressions.makeIndexExpression(productsBeforeIndices2Prime.get(i), productBeforeIndexType);
			productsBeforeIndexExpressionSets2Prime.add(indexExpression2Prime);
		}
	
		List<Expression> forAllIndexExpressionSets = new ArrayList<>();
		forAllIndexExpressionSets.addAll(productsBeforeIndexExpressionSetsPrime);
		forAllIndexExpressionSets.addAll(productsBeforeIndexExpressionSets2Prime);
		
		// Construct the nested for all statement.
		Expression forAll = implication;
		for (int i = forAllIndexExpressionSets.size()-1; i >= 0; i--) {
			Expression forAllIndexExpressionSet = forAllIndexExpressionSets.get(i);
			forAll = ForAll.make(forAllIndexExpressionSet, forAll);
		}
		
		Expression forAllEvaluated = context.getTheory().evaluate(forAll, context);
		
		if (Expressions.TRUE.equals(forAllEvaluated)) {		
			result = true;
		}
		
		return result;
	}
	
	private static Expression quantifyE(Expression E, Expression quantifier) {
		Expression result = Expressions.apply(quantifier.getFunctor(), 
				getIntensionalSet(quantifier).setHead(E));
		
		return result;
	}
	
	private static Expression applyInversion(Expression summationIndexedByFunction, Expression summationIndexFunctionName, FunctionType summationIndexFunctionType, 
			List<Expression> originalQuantifierOrder, List<Expression> inversionQuantifierOrder, Context context) {
		Expression result;
		
		int indexOfSummationIndexedByFunction = inversionQuantifierOrder.indexOf(summationIndexedByFunction);	
		// NOTE: only products will bubble up before the summation.
		List<Expression> productsBefore = inversionQuantifierOrder.subList(0, indexOfSummationIndexedByFunction);
		
		Expression lastQuantifierHead = getHead(originalQuantifierOrder.get(originalQuantifierOrder.size()-1));		
		Context innerContext = context;
		for (Expression quantifier : originalQuantifierOrder) {
			innerContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(getIndexExpressions(quantifier), innerContext);
		}
		Context lastQuantifierHeadContext = innerContext;
		
// TODO - remove temporary hack which collapses the function's argument domains
// due to all the domain arguments being treated as constants. Instead should be using
// set expression types on singletons.				
		// Determine which domain arguments from the summation function can be collapsed
		List<Expression> productsBeforeIndices = new ArrayList<>();
		for (Expression productBefore : productsBefore) {
			productsBeforeIndices.add(getIndexAndType(productBefore).first);
		}
		Set<Integer> domainArgsToRemove = new HashSet<>(); 
		Set<Integer> domainArgsHaveVar  = new HashSet<>();
		new SubExpressionsDepthFirstIterator(lastQuantifierHead).forEachRemaining(e -> {
			if (e.hasFunctor(summationIndexFunctionName)) {
				for (int i = 0; i < e.numberOfArguments(); i++) {				
					if (lastQuantifierHeadContext.getTheory().isVariable(e.get(0), lastQuantifierHeadContext)) {
						domainArgsHaveVar.add(i);
					}
					if (productsBeforeIndices.contains(e.get(i)) || Util.thereExists(new SubExpressionsDepthFirstIterator(e.get(i)), es -> productsBeforeIndices.contains(es))) {
						domainArgsToRemove.add(i);
					}
				}
			}
		});
		
		// Remove arg positions that were not populated by a variable.
		for (int i = 0; i < summationIndexFunctionType.getArity(); i++) {
			if (!domainArgsHaveVar.contains(i)) {
				domainArgsToRemove.add(i);
			}
		}
		
		List<Expression> argTypes = new ArrayList<>();
		for (int i = 0; i < summationIndexFunctionType.getArity(); i++) {
			if (!domainArgsToRemove.contains(i)) {
				argTypes.add(parse(summationIndexFunctionType.getArgumentTypes().get(i).getName()));
			}
		}
		
		Expression codomainType = parse(summationIndexFunctionType.getCodomain().getName());
		Expression summationIndexReducedType;
		if (argTypes.size() == 0) {
			summationIndexReducedType = codomainType;
		}
		else {
			summationIndexReducedType = FunctionType.make(codomainType, argTypes);
		}
		
		Expression summationIndex = IndexExpressions.makeIndexExpression(summationIndexFunctionName, summationIndexReducedType);		
		
		Expression phi = lastQuantifierHead.replaceAllOccurrences(e -> {
			Expression r = e;
			if (e.hasFunctor(summationIndexFunctionName)) {
				List<Expression> argsToKeep = new ArrayList<>();
				for (int i = 0; i < e.numberOfArguments(); i++) {
					if (!domainArgsToRemove.contains(i)) {
						argsToKeep.add(e.get(i));
					}
				}
				if (argsToKeep.size() > 0) {
					r = Expressions.apply(summationIndexFunctionName, argsToKeep);
				}
				else {
					r = summationIndexFunctionName;
				}
			}
			return r;
		}, lastQuantifierHeadContext);
		
		Expression summationHead = phi;
		for (int i = indexOfSummationIndexedByFunction+1; i < inversionQuantifierOrder.size(); i++) {
			Expression quantifier               = inversionQuantifierOrder.get(i);
			Expression quantifierIntensionalSet = IntensionalSet.intensionalMultiSet(
					getIndexExpressions(quantifier), summationHead, getCondition(quantifier));
			
			summationHead = Expressions.apply(quantifier.getFunctor(), quantifierIntensionalSet);
		}
		
		Expression innerSummation = IntensionalSet.intensionalMultiSet(
				new ExtensionalIndexExpressionsSet(summationIndex), 
				summationHead, getCondition(summationIndexedByFunction));
		
		result = Expressions.apply(FunctorConstants.SUM, innerSummation);
		
		for (int i = productsBefore.size() -1; i >= 0; i--) {
			Expression product = productsBefore.get(i);
			product = IntensionalSet.intensionalMultiSet(
					getIndexExpressions(product), result, getCondition(product));
			
			result = Expressions.apply(FunctorConstants.PRODUCT, product);
		}
				
		return result;
	}
	
	private static void collectQuantifiers(Expression expression, List<Expression> quantifiers) {
		if (isFunctionOnIntensionalSetWithSingleIndex(null, expression)) {
			quantifiers.add(expression);
			collectQuantifiers(getHead(expression), quantifiers);
		}
	}
	
	private static boolean isSummationIndexedByFunctionOfQuantifiers(Expression expression, Context context) {
		boolean result = false;
		
		if (isFunctionOnIntensionalSetWithSingleIndex(FunctorConstants.SUM, expression)) {
			Pair<Expression, Expression> indexAndType = getIndexAndType(expression);
			if (indexAndType.second != null && indexAndType.second.hasFunctor(FunctorConstants.FUNCTION_TYPE)) {
				// A summation indexed by a function of at least 1 nested quantifier, the first must be a product
				// in order to possibly move it before the summation.
				result = isFunctionOnIntensionalSetWithSingleIndex(FunctorConstants.PRODUCT, getHead(expression));
			}					
		}
		
		return result;
	}
	
	private static boolean isFunctionOnIntensionalSetWithSingleIndex(Object functor, Expression expression) {
		boolean result = false;
		if (((functor == null && Expressions.isFunctionApplicationWithArguments(expression)) || expression.hasFunctor(functor)) 
				&& expression.numberOfArguments() == 1) {
			Expression expressionArg1 = expression.get(0);
			if (Sets.isIntensionalSet(expressionArg1)) {
				IntensionalSet intensionalSet           = (IntensionalSet) expressionArg1;
				IndexExpressionsSet indexExpressionsSet = intensionalSet.getIndexExpressions();
				List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);			
				if (indices.size() == 1) {
					result = true;
				}
			}
		}
		return result;
	}
	
	private static IntensionalSet getIntensionalSet(Expression functionOnIntensionalSet) {
		IntensionalSet result = (IntensionalSet) functionOnIntensionalSet.get(0);
		return result;
	}
	
	private static IndexExpressionsSet getIndexExpressions(Expression functionOnIntensionalSet) {
		IndexExpressionsSet result = getIntensionalSet(functionOnIntensionalSet).getIndexExpressions();
		return result;
	}	
	
	private static Expression getHead(Expression functionOnIntensionalSet) {
		Expression result = getIntensionalSet(functionOnIntensionalSet).getHead();
		return result;
	}
	
	private static Expression getCondition(Expression functionOnIntensionalSet) {
		Expression result = getIntensionalSet(functionOnIntensionalSet).getCondition();
		return result;
	}
	
	private static Pair<Expression, Expression> getIndexAndType(Expression functionOnIntensionalSet) {
		IndexExpressionsSet indexExpressionsSet   = getIndexExpressions(functionOnIntensionalSet);		
		List<Expression> indexExpressionsWithType = IndexExpressions.getIndexExpressionsWithType(indexExpressionsSet);
		if (indexExpressionsWithType.size() != 1) {
			throw new UnsupportedOperationException("Currently only support singular indices");
		}	
		Pair<Expression, Expression> result = IndexExpressions.getIndexAndDomain(indexExpressionsWithType.get(0));
		return result;
	}
	
	private static Pair<Expression, FunctionType> getIndexAndFunctionType(Expression functionOnIntensionalSet, Context context) {
		IndexExpressionsSet indexExpressionsSet = getIndexExpressions(functionOnIntensionalSet);		
		List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);
		if (indices.size() != 1) {
			throw new UnsupportedOperationException("Currently only support singular indices");
		}
		Expression index              = indices.get(0);
		Context intensionalSetContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressionsSet, context);
		Type type                     = GrinderUtil.getType(index, intensionalSetContext);
		
		FunctionType functionType = null;
		if (type instanceof FunctionType) {
			functionType = (FunctionType) type;
		}
		
		Pair<Expression, FunctionType> result = new Pair<>(index, functionType);
		return result;
	}
	
	private static Expression replaceAll(Expression expression, List<Expression> targetVariables, List<Expression> replacementVariables, Context context) {
		Expression result = expression.replaceAllOccurrences(e -> {
			Expression r = e;
			int idx = targetVariables.indexOf(e);
			if (idx >= 0) {
				r = replacementVariables.get(idx);
			}
			return r;
		}, context);
		
		return result;
	}
}
