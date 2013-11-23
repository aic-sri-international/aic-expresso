/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.base.Pair;

/**
 * Default implementation of R_card_disjunction(| F |_X, quantification).
 * 
 * @author oreilly
 *
 */
@Beta
public class CardinalityDisjunction extends AbstractHierarchicalRewriter implements CardinalityRewriter {

	private SortPair sortPair = new SortPair();
	
	public CardinalityDisjunction() {
	}
	
	@Override
	public String getName() {
		return R_card_disjunction;
	}
	
	public SortPair getSortPair() {
		return sortPair;
	}
	
	public void setSortPair(SortPair sortPair) {
		this.sortPair = sortPair;
	}
	
	/**
	 * @see CardinalityRewriter#R_card_disjunction
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, (| F |_x, quantification)
		if (!(Tuple.isTuple(expression) &&
			  Tuple.size(expression) == 2)
			  ) {
			throw new IllegalArgumentException("Invalid input argument expression, expect (| F |_X, quantification):"+expression);
		}
		Expression cardinalityOfIndexedFormulaExpression = Tuple.get(expression, 0);
		Expression quantificationSymbol                  = Tuple.get(expression, 1);
		// 
		CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(cardinalityOfIndexedFormulaExpression);
		// | {(on x1,..., xn)(x1, ..., xn) | F} |
		Expression       intensionalSet   = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f                = IntensionalSet.getCondition(intensionalSet);
		List<Expression> indices          = IntensionalSet.getIndexExpressions(intensionalSet);
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		Expression[]     indexExpressionsAsArray   = indices.toArray(new Expression[indices.size()]);
		RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIntensionalSetIndices(intensionalSet, process);
		
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol: " + quantificationSymbol);
		}
		
		if (!Or.isDisjunction(f)) {
			throw new IllegalArgumentException("Input argument assumption F is of the form F1 or F2 does not hold:"+f);
		}
		
		Expression cardIndexX = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressionsAsArray);
		List<Pair<Set<Expression>, List<Expression>>> independentProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indexExpressions, subProcess);
		
		if ( independentProblems.isEmpty() ) {
			//
			// Begin main logic:
			// Assume F is of the form F1 or F2
			// | F1 or F2 |_x = | F1 |_x + | F2 |_x - | F1 and F2 |_x
			// but we do not compute it like that, see below for optimized version.
			
			// Separate the disjuncts that are independent of the index and factor them out
			Trace.log("(F1, F2) <- split_disjuncts_on_x(F)");
			Pair<Expression, Expression> pair = CardinalityUtil.separateIndependentAndDependent(f, IndexExpressions.getIndices(indexExpressions), Expressions.FALSE, subProcess);
			Expression independentDisjunction = pair.first;
			Expression dependentDisjunction   = pair.second;
			if ( !Expressions.FALSE.equals(independentDisjunction) ) {
				Trace.log("if F1 contains all the disjuncts independent of X (not empty)");
				Trace.log("    return R_normalize(if F1 then ||X|| else R_card(|F2|_X, quantification))");
				Expression cardDependentDisjunction = null;
				if ( Expressions.FALSE.equals(dependentDisjunction) ) {
					cardDependentDisjunction = Expressions.ZERO;
				} 
				else {
					Expression dependentDisjunctionCard = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(dependentDisjunction, indexExpressionsAsArray);
					cardDependentDisjunction = process.rewrite(R_card,
													CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(dependentDisjunctionCard, quantification));
				}
				result = IfThenElse.make(independentDisjunction, cardIndexX, cardDependentDisjunction);
				result = process.rewrite(R_normalize, result);
			} 
			else {
				Trace.log("if all disjuncts of F have some index variable occurring in them");
				result = rewriteDisjunctionIndexInAllDisjuncts(f, indexExpressionsAsArray, quantification, process);
			}
		}
		else {
			Trace.log("if F is a disjunction which can be partitioned into a few subproblems:");
			Pair<Set<Expression>, List<Expression>> indexlessProblem = null;
			if  ( independentProblems.get(0).first.size() == 0 ) {
				Trace.log("    // F has a sub-disjunction D that is independet of all index variables");
				indexlessProblem = independentProblems.get(0);
				independentProblems.remove(0);
			}
			Pair<Set<Expression>, List<Expression>> firstProblem = independentProblems.get(0);
			Expression firstDisjunction = Or.make(firstProblem.second);
			Trace.log("    D_1 = R_top_simplify(D_1)");
			firstDisjunction = process.rewrite(R_top_simplify, firstDisjunction);
			
			Expression firstDisjunctionCard         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(firstDisjunction, firstProblem.first.toArray(new Expression [0]));
			Expression computedFirstDisjunctionCard = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(firstDisjunctionCard, quantification));
			
			Expression cardinalityOfTheRest = null;
			if ( independentProblems.size() == 1 ) {
				Trace.log("    // F has one sub-disjunction D1 containing all index variables");
				Trace.log("    R <- R_card(| D_1 |_X, quantification)");
				cardinalityOfTheRest = computedFirstDisjunctionCard;
			} 
			else {
				Trace.log("    // I_1 and I_2 are two partitions of X, and D_1 and D_2 are two partitions of the disjuncts in F, and the index variables in D_1 and D_2 are I_1 and I_2 respectively.");
				Expression firstIndexCard = CardinalityUtil.makeCardinalityOfIndexExpressions(firstProblem.first.toArray(new Expression [0]));

				Pair<Set<Expression>, List<Expression>> secondProblem = independentProblems.get(1);
				Expression secondIndexCard               = CardinalityUtil.makeCardinalityOfIndexExpressions(secondProblem.first.toArray(new Expression [0]));
				Expression secondDisjunction             = Or.make(secondProblem.second);	
				Trace.log("    D_2 = R_top_simplify(D_2)");
				secondDisjunction                        = process.rewrite(R_top_simplify, secondDisjunction);
				Trace.log("    R <- R_normalize(R_card(| D_1 |_I_1, quantification)*||I_2|| + R_card(| D_2 |_I2, quantification)*||I_1|| - R_card(| D_1 |_I_1, quantification)* R_card(| D_2 |_I_2, quantification))");
				Expression secondDisjunctionCard         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(secondDisjunction, secondProblem.first.toArray(new Expression [0]));
				Expression computedSecondDisjunctionCard = process.rewrite(R_card,
																CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(secondDisjunctionCard, quantification));
				
				Expression term1 = Times.make(Arrays.asList(secondIndexCard, computedFirstDisjunctionCard));
				Expression term2 = Times.make(Arrays.asList(firstIndexCard, computedSecondDisjunctionCard));

				Expression term3 = Times.make(Arrays.asList(computedFirstDisjunctionCard, computedSecondDisjunctionCard));
				Expression term4 = Plus.make(Arrays.asList(term1, term2));
				
				cardinalityOfTheRest = Minus.make(term4, term3);
				cardinalityOfTheRest = process.rewrite(R_normalize, cardinalityOfTheRest);
			}
			
			if ( indexlessProblem == null ) {
				Trace.log("    return R");
				result = cardinalityOfTheRest;
			} 
			else {
				Trace.log("    return R_normalize(if D then ||X|| else R)");
				result = IfThenElse.make(Or.make(indexlessProblem.second), cardIndexX, cardinalityOfTheRest);
				result = process.rewrite(R_normalize, result);
			}
		}
		
		return result;
	}
	
	private Expression rewriteDisjunctionIndexInAllDisjuncts(Expression f, Expression[] indexExpressions, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression result = null;
		Expression cardIndices = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressions);
		
		Trace.log("F1 <- first disjunct in F");
		Expression f1 = CardinalityUtil.getF1FromDisjunction(f);
		Trace.log("F2 <- remaining disjuncts in F");
		Expression f2 = CardinalityUtil.getF2FromDisjunction(f);		
		
		Trace.log("F1 <- R_top_simplify(F1)");
		f1 = process.rewrite(R_top_simplify, f1);
		Trace.log("F2 <- R_top_simplify(F2)");
		f2 = process.rewrite(R_top_simplify, f2);
		
		if (quantification == CardinalityRewriter.Quantification.FOR_ALL) {
			Trace.log("if quantification is \"for all\"");
			Trace.log("    return R_normalize(if R_card(| R_top_simplify_conjunction(not F1 and not F2)  |_X, \"there exists\") > 0 then 0 else ||X||)");
			Expression notF1AndNotF2 = CardinalityUtil.makeAnd(CardinalityUtil.makeNot(f1), CardinalityUtil.makeNot(f2)); 
			notF1AndNotF2 = process.rewrite(R_top_simplify_conjunction, notF1AndNotF2);
			
			Expression cardNotF1AndNotF2    = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(notF1AndNotF2, indexExpressions);
			Expression resultCard1          = process.rewrite(R_card,
													CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardNotF1AndNotF2, CardinalityRewriter.Quantification.THERE_EXISTS));
			Expression resultCard1NotEqual0 = Expressions.make(FunctorConstants.GREATER_THAN, resultCard1, Expressions.ZERO); 
			Expression ifThenElse           = IfThenElse.make(resultCard1NotEqual0, Expressions.ZERO, cardIndices);
			
			result = process.rewrite(R_normalize, ifThenElse);
		} 
		else {			
			Trace.log("(F1, F2) <- sort_pair({}, {})", f1, f2);
			Pair<Expression, Expression> sortedPair = sortPair.sort(f1, f2);
			f1 = sortedPair.first;
			f2 = sortedPair.second;
			Trace.log("// f1 = {}, f2 = {}", f1, f2);
			
			Expression cardF1x = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(f1, indexExpressions);
			Expression cardF2x = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(f2, indexExpressions);
			// Need to do this to get | type(X) | converted to its known value, e.g.: 10
			cardIndices = process.rewrite(R_normalize, cardIndices);
			
			Trace.log("N1 <- R_card(| F1 |_X, quantification)");
			Expression n1 = process.rewrite(R_card,
								CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardF1x, quantification));
			
			if (n1.equals(Expressions.ZERO)) {
				Trace.log("if N1 = 0");
				Trace.log("    R_card(| F2 |_X, quantification) // | F1 and F2 |_X is 0");
				result = process.rewrite(R_card,
							CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardF2x, quantification));
			}
			else if (n1.equals(cardIndices)) {
				Trace.log("if N1 = ||X||");
				Trace.log("    return ||X|| // | F2 |_X = | F1 and F2 |_X and cancel out");
				result = cardIndices;
			}
			
			Expression n2 = null;
			if (result == null) {
				Trace.log("N2 <- R_card(| F2 |_X, quantification)");
				n2 = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardF2x, quantification));
				if (n2.equals(Expressions.ZERO)) {
					Trace.log("if N2 = 0");
					Trace.log("    return N1 // | F1 and F2 |_X is 0");
					result = n1;
				}
				else if (n2.equals(cardIndices)) {
					Trace.log("if N2 = ||X||");
					Trace.log("    return ||X|| // N1 = | F1 |_X = | F1 and F2 |_X and cancel out");
					result = cardIndices;
				}
			}
			
			if (result == null && quantification == CardinalityRewriter.Quantification.THERE_EXISTS) {
				Trace.log("if quantification is \"there exists\"");
				// there is no need to compute N3, since it is enough that there is x for either F1 or F2
				Trace.log("    return R_normalize(if N1 > 0 or N2 > 0 then ||X|| else 0)");
				Expression n1NotEqual0     = Expressions.make(FunctorConstants.GREATER_THAN, n1, Expressions.ZERO);
				Expression n2NotEqual0     = Expressions.make(FunctorConstants.GREATER_THAN, n2, Expressions.ZERO);
				Expression n1orn2NotEqual0 = CardinalityUtil.makeOr(n1NotEqual0, n2NotEqual0);
				Expression ifThenElse      = IfThenElse.make(n1orn2NotEqual0, cardIndices, Expressions.ZERO);
				
				result = process.rewrite(R_normalize, ifThenElse);
			}
			
			if (result == null) {
				// quantification is guaranteed to be "none" because otherwise it will have returned above
				if (quantification != CardinalityRewriter.Quantification.NONE) {
					throw new IllegalStateException("Have a quantification other than 'none', which is meant to be guaranteed:"+quantification);
				}
				
				Trace.log("N3 <- R_card( | R_top_simplify_conjunction(F1 and F2) |_X, \"none\" )");
				Expression n3 = null;
				Expression f1Andf2                        = CardinalityUtil.makeAnd(f1, f2);
				Expression topSimplifiedF1AndF2           = process.rewrite(R_top_simplify_conjunction, f1Andf2);
				Expression cardTopSimplifiedF1AndF2IndexX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(topSimplifiedF1AndF2, indexExpressions);
			
				n3 = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardTopSimplifiedF1AndF2IndexX, quantification));
				
				Trace.log("return R_normalize(N1 + N2 - N3)");
				Expression n1PlusN2 = Plus.make(Arrays.asList(n1, n2));
				Expression n1PlusN2MinusN3 = Expressions.apply(FunctorConstants.MINUS, n1PlusN2, n3);
				
				result = process.rewrite(R_normalize, n1PlusN2MinusN3);
			}
		}
		
		return result;
	}
}
