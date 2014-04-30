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

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.Util;

/**
 * Default implementation of R_card_conjunction_of_disequalities(| F |_X, quantification).
 * 
 * @author saadati
 *
 */
@Beta
public class CardinalityConjunctionOfDisequalities  extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	public CardinalityConjunctionOfDisequalities() {
	}

	@Override
	public String getName() {
		return R_card_conjunction_of_disequalities;
	}

	/**
	 * @see CardinalityRewriter#R_card_conjunction_of_disequalities
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, (| F |_X, quantification)
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
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
		RewritingProcess subProcess       = GrinderUtil.extendContextualVariablesWithIntensionalSetIndices(intensionalSet, process);
		
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol:"+quantificationSymbol);
		}
		result = rewriteCardinalityOfDisequalities(f, indexExpressions, quantification, subProcess);
		return result;
	}
	
	
	// This is based on the new idea: We delay using SumOverOneVariable up to this point.
	protected Expression rewriteCardinalityOfDisequalities(Expression conjunction, List<Expression> indexExpressions, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression result = null;
		if ( indexExpressions.size() == 1 ) {
			Trace.log("if X = {x}");
			Expression indexExpressionX = indexExpressions.get(0);
			Expression indexX      = IndexExpressions.getIndex(indexExpressionX);
			Expression cardIndex   = CardinalityUtil.makeCardinalityOfIndexExpressions(Util.list(indexExpressionX));
			Set<Expression> t1ToTk = extractT1ToTk(conjunction, indexX, process);
			if ( t1ToTk.contains(indexX) ) { // There has been a conjunct of the form X!=X 
				result = Expressions.ZERO;
			} 
			else {
				Expression cardIndexXGreaterThanZero = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.GREATER_THAN, cardIndex, Expressions.ZERO);
				Expression cardIndexXGreaterThanK    = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.GREATER_THAN, cardIndex, DefaultSymbol.createSymbol(t1ToTk.size()));
				if (quantification == CardinalityRewriter.Quantification.FOR_ALL &&
					(GrinderConfiguration.isAssumeDomainsAlwaysLarge() || process.rewrite(R_normalize, cardIndexXGreaterThanZero).equals(Expressions.TRUE))) {
					Trace.log("    if quantification is \"for all\" and (ASSUME_DOMAIN_ALWAYS_LARGE or |type(x)| > 0)");
					Trace.log("        return 0");
					result = Expressions.ZERO;
				} 
				else if (quantification == CardinalityRewriter.Quantification.THERE_EXISTS &&
						(GrinderConfiguration.isAssumeDomainsAlwaysLarge() || process.rewrite(R_normalize, cardIndexXGreaterThanK).equals(Expressions.TRUE))) {
					Trace.log("    if quantification is \"there exists\" and (ASSUME_DOMAIN_ALWAYS_LARGE or |type(x)| > k)");
					Trace.log("        return R_normalize(|type(x)|)");
					result = process.rewrite(R_normalize, cardIndex);
				} 
				else {
					Trace.log("    return R_normalize(|type(x)| - R_cardExtensionalSet(|{t1,...,tk}|)");
					Expression extensionalSet                  = ExtensionalSet.makeUniSet(new ArrayList<Expression>(t1ToTk));
					Expression cardExtensionalSet              = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, extensionalSet);
					Expression resultRewriteCardExtensionalSet = process.rewrite(R_cardExtensionalSet, cardExtensionalSet);
					Expression cardIndexXMinusRewriteResult    = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.MINUS, cardIndex, resultRewriteCardExtensionalSet);
					result = process.rewrite(R_normalize, cardIndexXMinusRewriteResult);
				}
			}
		} 
		else {
//			Pair<Expression, Set<Expression>> pair = null;
//			pair = possibleToEliminateAnyIndex(conjunction, indices);
//			if ( pair != null ) {
//				Trace.log("// Found an index X to eliminate");				
//				Trace.log("    return (|type(X|)  - A) * |F'|_Z");
//				Trace.log("    // X is the index to be eliminated");
//				Trace.log("    // F' is obtained from F by eliminating all disequalities of F where X occurs in");
//				Trace.log("    // A is the set of all free variabels and constants in F and any other index variable Y have been replaced by new index variable Y'");
//				Trace.log("    // Z is the set containing any new index variable Y'");
//				result = eliminateIndex(conjunction, indices, pair.first, pair.second, quantification, process);
			Trace.log("if X = {x1, ..., xn}");
			result = rewriteUsingSumOverOneVariable(conjunction, indexExpressions, quantification, process);
			//result = rewriteByConvertingOneDisequalityToEquality(conjunction, indices, quantification, process);
		}
		return result;
	}
	
	private Set<Expression> extractT1ToTk(Expression f, Expression indexX, RewritingProcess process) {
		// Note: When calling this I already know this is a conjunction of inequalities on X
		Set<Expression>  t1ToTk    = new LinkedHashSet<Expression>();
		List<Expression> conjuncts = Expressions.takeFormulaAsConjunctionAndReturnConjuncts(f);

		for (Expression conjunct : conjuncts) {
			int tiIndex = -1;
			Expression simplified = process.rewrite(R_normalize, conjunct);
			if (Expressions.TRUE.equals(simplified)) {
				continue;
			} 
			else if ( Expressions.FALSE.equals(simplified) ) { // conjunct is X!=X
				t1ToTk.add(indexX);
				break;
			} 
			else if (simplified.get(0).equals(indexX)) {
				tiIndex = 1;
			}
			else {
				tiIndex = 0;
			}
			t1ToTk.add(simplified.get(tiIndex));
		}

		return t1ToTk;
	}	
	
	private Expression rewriteUsingSumOverOneVariable(Expression conjunction, List<Expression> indices, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression result = null;
		Trace.log("    return R_sum_over_one_variable(sum_{x1:True} R_card( | F |_{x2, ..., xn}, quantification ))");
		Expression x1            = indices.get(0);
		int        indexesSize   = indices.size();
		Expression cardFMinusX1  = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(conjunction, indices.subList(1, indexesSize).toArray(new Expression[indexesSize-1]));
		Expression cardConjArgs  = CardinalityUtil.argForCardinalityConjunctionCall(cardFMinusX1, quantification);
		Expression cardResult    = process.rewrite(R_card_conjunction, cardConjArgs);
		Expression sumCardResult = CardinalityUtil.makeSummationExpression(x1, Expressions.TRUE, cardResult);
		
		result = process.rewrite(R_sum_over_one_variable, sumCardResult);
		return result;
	}

	@SuppressWarnings("unused")
	private Expression rewriteByConvertingOneDisequalityToEquality( Expression conjunction, List<Expression> indices, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression result = null;
		Trace.log("    // F is of the form 'x!=t and Phi'");
		Trace.log("    return R_card( | Phi |_X) - R_card( | Phi[x/t] |_X\\{x} )");
		List<Expression> conjuncts = new ArrayList<Expression>();
		if ( And.isConjunction(conjunction) ) {
			conjuncts.addAll(conjunction.getArguments());
		}
		else {
			conjuncts.add(conjunction);
		}
		Expression first = conjuncts.get(0);
		conjuncts.remove(0);
		Expression allButFirst             = And.make(conjuncts);
		Expression allButFirstCard         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(allButFirst, indices.toArray(new Expression[indices.size()]));
		Expression allButFirstCardComputed = process.rewrite(R_card_conjunction, 
												CardinalityUtil.argForCardinalityConjunctionCall(allButFirstCard, quantification));
		
		Expression negateFirst = process.rewrite(R_move_not_in, Not.make(first));
		conjuncts.add(0, negateFirst);
		
		Expression allAndNotFirst             = And.make(conjuncts);
		Expression allAndNotFirstCard         = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(allAndNotFirst, indices.toArray(new Expression[indices.size()]));
		Expression allAndNotFirstCardComputed = process.rewrite(R_card_conjunction,
													CardinalityUtil.argForCardinalityConjunctionCall(allAndNotFirstCard, quantification));
		
		result = Minus.make(allButFirstCardComputed, allAndNotFirstCardComputed);
		result = process.rewrite(R_normalize, result);
		return result;
	}
	
//	private Pair<Expression, Expression> getLiteralArguments(Expression literal) {
//		Pair<Expression, Expression> result = new Pair<Expression, Expression>();
//		if ( literal.hasFunctor(Equality.FUNCTOR) || literal.hasFunctor(Disequality.FUNCTOR) ) {
//			result.first = literal.get(0);
//			result.second = literal.get(1);
//		} 
//		else if ( literal.hasFunctor(Not.FUNCTOR) ) {
//			result = getLiteralArguments(literal.get(0));
//		} 
//		else {
//			throw new IllegalArgumentException("Expression " + literal + " is not a literal");
//		}
//		return result;
//	}
//	
//	private Pair<Expression, Set<Expression>> possibleToEliminateAnyIndex(Expression conjunction, List<Expression> indices) {
//		Pair<Expression, Set<Expression>> result = null;
//		HashMap<Expression, Set<Expression>> map = new LinkedHashMap<Expression, Set<Expression>>();
//		for (Expression index: indices) {
//			map.put(index, new LinkedHashSet<Expression>());
//			map.get(index).add(index);
//		}
//		Set<Expression> allSymbols = new LinkedHashSet<Expression>();
//		List<Expression> conjuncts = new ArrayList<Expression>();
//		if ( conjunction.hasFunctor(And.FUNCTOR) ) {
//			conjuncts.addAll(conjunction.getArguments());
//		} 
//		else {
//			conjuncts.add(conjunction);			
//		}
//
//		for (Expression dis: conjuncts) {
//			Pair<Expression, Expression> pair = getLiteralArguments(dis);
//			if ( indices.contains(pair.first) ) {
//				map.get(pair.first).add(pair.second);
//			}
//			if ( indices.contains(pair.second) ) {
//				map.get(pair.second).add(pair.first);
//			}
//			allSymbols.add(pair.first);
//			allSymbols.add(pair.second);
//		}
//		for (Expression index: map.keySet()) {
//			Set<Expression> others = map.get(index);
//			if ( others.equals(allSymbols) ) {
//				others.remove(index);
//				result = new Pair<Expression, Set<Expression>>(index, others);
//				break;
//			}
//		}
//		return result;
//	}
//	
//	private Expression makeUniqueVariable(Symbol var, Expression expression, RewritingProcess process) {
//		return Expressions.primedUntilUnique(var, expression, process);
//	}
//	
//	private Expression eliminateIndex(Expression conjunction, List<Expression> indices, Expression index, Set<Expression> others, CardinalityRewriters.Quantification quantification, RewritingProcess process) {
//		Expression result = null;
//		ArrayList<Expression> conjunctsToKeep = new ArrayList<Expression>();
//		List<Expression> conjuncts = new ArrayList<Expression>();
//		if ( conjunction.hasFunctor(And.FUNCTOR) ) {
//			conjuncts.addAll(conjunction.getArguments());
//		} 
//		else {
//			conjuncts.add(conjunction);			
//		}
//		
//		for (Expression dis: conjuncts) {
//			Pair<Expression, Expression> pair = getLiteralArguments(dis);
//			if ( !index.equals(pair.first) && !index.equals(pair.second) ) {
//				conjunctsToKeep.add(dis);
//			}
//		}
//		others.removeAll(indices);
//		Expression theSet = ExtensionalSet.makeUniSet(Lists.newArrayList(others.toArray(new Expression[others.size()])));
//		Expression setCardinality = Expressions.make(FunctorConstants.CARDINALITY, theSet);
//		Expression setCardComputed = cardinalityRewriters.cardinalityExtensionalSet(setCardinality, process);
//		Expression difference = Minus.make(CardinalityUtil.makeCardinalityOfIndexExpressions(index), setCardComputed);
//		Expression originalSet = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(conjunction, indices.toArray(new Expression[indices.size()]));
//		
//		Expression newConjunction = And.make(conjunctsToKeep);
//		ArrayList<Expression> newIndices = new ArrayList<Expression>();
//		for (Expression idx: indices) {
//			if ( !idx.equals(index) ) {
//				Expression newIdx = makeUniqueVariable((Symbol)idx, originalSet, process);
//				Expression cardIdx = CardinalityUtil.makeCardinalityOfIndexExpressions(idx);
//				Expression cardNewIdx = Minus.make(cardIdx, Expressions.ONE);
//				//CardinalityUtil.setCardinalityOfIndexExpression(newIdx, cardNewIdx);
//				newConjunction = Substitute.replace(newConjunction, idx, newIdx, true, process);
//				newIndices.add(newIdx);
//			}
//		}
//
//		Expression newSetCard = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(newConjunction, newIndices.toArray(new Expression[newIndices.size()]));
//		
//		Expression card = cardinalityRewriters.cardinalityConjunction(newSetCard, quantification, process);
//		result = Times.make(Lists.newArrayList(difference, card));
//		result = cardinalityRewriters.simplify(result, process);
//		return result;
//	}	
}
