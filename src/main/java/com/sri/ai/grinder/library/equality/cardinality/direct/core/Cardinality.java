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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;


/**
 * Default implementation of R_card(| F |_X).
 * 
 * @author oreilly
 *
 */
@Beta
public class Cardinality extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	public Cardinality() {
	}
	
	@Override
	public String getName() {
		return R_card;
	}

	/**
	 * @see CardinalityRewriter#R_card
	 */
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if ( Tuple.isTuple(expression) && Tuple.size(expression) == 2 ) {
			Expression cardinalityOfIndexedFormulaExpression  = Tuple.get(expression, 0);
			Expression quantificationSymbol                   = Tuple.get(expression, 1);
			CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
			result = rewrite(cardinalityOfIndexedFormulaExpression, quantification, process);
		} 
		else {
		
			// Assert input arguments, | F |_x
			// 
			CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(expression);
			
			if (Sets.isEmptySet(expression.get(0))) {
				result = Expressions.ZERO;
			}
			else {
				// | {(on x1,..., xn) (x1, ..., xn) | F} |
				Expression       intensionalSet   = expression.get(0);
				Expression       f                = IntensionalSet.getCondition(intensionalSet);
				List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(intensionalSet);
				
				Trace.log("F <- R_top_simplify(F)");
				f = process.rewrite(CardinalityRewriter.R_top_simplify, f);
			
				if (indexExpressions.size() == 0) {
					Trace.log("if n = 0");
					Trace.log("    return R_simplify(if F then 1 else 0)");
					Expression ifThenElse = IfThenElse.make(f, Expressions.ONE, Expressions.ZERO);
					result = process.rewrite(CardinalityRewriter.R_simplify, ifThenElse);
				} 
				else {
					Trace.log("if n > 0");
					// Note: the input f may have been simplified.
					if ( negationHasLessNumberOfDisjuncts(f) ) {
						Trace.log("    if negationHasLessNumberOfDisjuncts(F):");
						Trace.log("        return R_simplify(||X|| - R_card( | not F |_X, \"none\" ) ) ");
						Expression[] indicesAsArray    = indexExpressions.toArray(new Expression[indexExpressions.size()]);
						Expression cardIndexX          = CardinalityUtil.makeCardinalityOfIndexExpressions(indicesAsArray);
						Expression negationCardinality = process.rewrite(R_card,
									CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
										CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(Not.make(f), indicesAsArray), 
										CardinalityRewriter.Quantification.NONE));
						Expression subtraction = Expressions.make(FunctorConstants.MINUS, cardIndexX, negationCardinality);
						result = process.rewrite(CardinalityRewriter.R_simplify, subtraction);
					}
					else {
						Trace.log("    if not negationHasLessNumberOfDisjuncts(F):");
						Trace.log("        return R_card( | F |_X, \"none\")");
						result = process.rewrite(R_card,
									CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
										expression, CardinalityRewriter.Quantification.NONE));
					}
				}
			}
		}
		return result;
	}
	
	protected boolean negationHasLessNumberOfDisjuncts(Expression formula) {
		boolean result = false;
		int fWorstCaseNumberOfDisjuncts, notFWorstCaseNumberOfDisjuncts;
		fWorstCaseNumberOfDisjuncts    = WorstCaseNumberOfDisjuncts.get(formula);
		notFWorstCaseNumberOfDisjuncts = WorstCaseNumberOfDisjuncts.get(Not.make(formula));
		Trace.log("    // worstCaseNumberOfDisjuncts(F) = " + fWorstCaseNumberOfDisjuncts);
		Trace.log("    // worstCaseNumberOfDisjuncts(not F) = " + notFWorstCaseNumberOfDisjuncts);
		if ( fWorstCaseNumberOfDisjuncts > notFWorstCaseNumberOfDisjuncts ) {
			result = true;
		}
		return result;
	}
	
	private Expression rewrite(Expression cardinalityOfIndexedFormulaExpression, CardinalityRewriter.Quantification quantification, RewritingProcess process) {
		Expression result = null;
		
		// 
		CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(cardinalityOfIndexedFormulaExpression);
		// | {(on x1,..., xn) (x1, ..., xn) | F} |
		Expression       intensionalSet = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f              = IntensionalSet.getCondition(intensionalSet);
		List<Expression> indices        = IntensionalSet.getIndexExpressions(intensionalSet);
		Expression[] indicesAsArray     = indices.toArray(new Expression[indices.size()]);

		
		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol: " + quantification);
		}	
		
		if (f.equals(Expressions.TRUE)) {
			Trace.log("if F is True");
			Trace.log("    return R_simplify(||X||)");
			Expression cardIndices = CardinalityUtil.makeCardinalityOfIndexExpressions(indicesAsArray);
			result = process.rewrite(CardinalityRewriter.R_simplify, cardIndices);
		}
		else if (f.equals(Expressions.FALSE)) {
			Trace.log("if F is False");
			Trace.log("    return 0");
			result = Expressions.ZERO;
		}
		else if (CardinalityUtil.areIndicesNotInF(indices, f, process)) {
			Trace.log("if x does not occur in F for any x in X");
			Trace.log("    return R_simplify(if F then ||X|| else 0)");
			Expression cardIndexX = CardinalityUtil.makeCardinalityOfIndexExpressions(indicesAsArray);
			Expression ifThenElse = IfThenElse.make(f, cardIndexX, Expressions.ZERO);
			result = process.rewrite(CardinalityRewriter.R_simplify, ifThenElse);
		}
		else if (CardinalityUtil.isConjunctionOrImpliedConjunction(f, process)) {
			Trace.log("if F is a conjunction // including F being a literal or a multi-equality");
			Trace.log("    return R_card_conjunction(|F|_X, quantification)");
			result = process.rewrite(R_card_conjunction,
						CardinalityUtil.argForCardinalityConjunctionCall(
								cardinalityOfIndexedFormulaExpression, quantification));
		}
		else if (Or.isDisjunction(f)) {
			Trace.log("if F is a disjunction");
			Trace.log("    return R_card_disjunction(|F|_X, quantification)");
			result = process.rewrite(R_card_disjunction,
						CardinalityUtil.argForCardinalityDisjunctionCall(
								cardinalityOfIndexedFormulaExpression, quantification));
		}
		else if (f.hasFunctor(FunctorConstants.NOT) && f.numberOfArguments() == 1) {
			Trace.log("if F is \"not G\"");
			Trace.log("    F' <- R_top_simplify(R_move_not_in(F))");
			Expression fPrime = process.rewrite(CardinalityRewriter.R_top_simplify, process.rewrite(CardinalityRewriter.R_move_not_in, f));
			Trace.log("    return R_card(|F'|_X, quantification)");
			Expression cardFPrimeIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(fPrime, indicesAsArray);
			result = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
								cardFPrimeIndexedByX, quantification));
		}
		else if (f.hasFunctor(FunctorConstants.IMPLICATION)) {
			Trace.log("if F is an implication of the form G => H");
			Trace.log("    return R_card_implication(|F|_X, quantification)");
			result = process.rewrite(R_card_implication,
						CardinalityUtil.argForCardinalityImplicationCall(
								cardinalityOfIndexedFormulaExpression, quantification));
		} 
		else if (f.hasFunctor(FunctorConstants.EQUIVALENCE)) {
			Trace.log("if F is an equivalence of the form G <=> H");
			Trace.log("    return R_card_equivalence(|F|_X, quantification)");
			result = process.rewrite(R_card_equivalence,
						CardinalityUtil.argForCardinalityEquivalenceCall(
								cardinalityOfIndexedFormulaExpression, quantification));
		} 
		else if (ForAll.isForAll(f) || ThereExists.isThereExists(f)) {
			Trace.log("if F is Q y : G");
			Trace.log("    return R_card( | R_top_quantifier_elimination(Q y : G) |_X, quantification)");
			Expression quantifierEliminated               = process.rewrite(CardinalityRewriter.R_top_quantifier_elimination, f);
			Expression cardQuantifierEliminatedIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(quantifierEliminated, indicesAsArray);
			
			result = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
								cardQuantifierEliminatedIndexedByX, quantification));
		}
		else {
			throw new IllegalArgumentException("F is unhandled:"+f);
		}
		
		return result;
	}
}
