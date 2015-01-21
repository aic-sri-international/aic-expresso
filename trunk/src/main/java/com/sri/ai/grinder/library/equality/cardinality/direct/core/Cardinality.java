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
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.AbstractCardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ModelCounting;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLUtil;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SolutionPostProcessing;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityOnTermsTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolTermTheory;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.tuple.Tuple;


/**
 * Default implementation of R_card(| F |_X).
 * 
 * @author oreilly
 *
 */
@Beta
public class Cardinality extends AbstractCardinalityRewriter {
	
	private Rewriter plainCardinality = GrinderUtil.usePlain? new DPLLGeneralizedAndSymbolic(new EqualityOnTermsTheory(new SymbolTermTheory()), new ModelCounting()) : null;

	private Expression usePlainCardinality(Expression cardinalityOfIndexedFormulaExpression, RewritingProcess process) {
		Expression result;
		
		Expression solution = plainCardinality.rewrite(cardinalityOfIndexedFormulaExpression, process);
		Expression simplifiedSolution = DPLLUtil.simplifySolutionUnderConstraint(solution, process.getContextualConstraint(), process);
		result = SolutionPostProcessing.fromSolutionToShorterExpression(simplifiedSolution, new EqualityOnTermsTheory(new SymbolTermTheory()), process);
		
//		System.out.println("Problem                  : " + cardinalityOfIndexedFormulaExpression);
//		System.out.println("Constraint               : " + process.getContextualConstraint());
//		System.out.println("Solution                 : " + solution);
//		System.out.println("Solution under constraint: " + simplifiedSolution);
//		System.out.println("Shortened solution       : " + result + "\n");
		
		return result;
	}

	public Cardinality() {
	}
	
	public Cardinality(CountsDeclaration countsDeclaration) {
		super(countsDeclaration);
	}
	
	@Override
	public String getName() {
		return R_card;
	}

	/**
	 * @see CardinalityRewriter#R_card
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if ( Tuple.isTuple(expression) && Tuple.size(expression) == 2 ) {
			Expression cardinalityOfIndexedFormulaExpression  = Tuple.get(expression, 0);
			Expression quantificationSymbol                   = Tuple.get(expression, 1);
			CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
			cardinalityOfIndexedFormulaExpression = CardinalityUtil.removeIfThenElsesFromFormula(cardinalityOfIndexedFormulaExpression, process);
			
			if (GrinderUtil.usePlain) {
				result = usePlainCardinality(cardinalityOfIndexedFormulaExpression, process);
			}
			else {
				result = rewrite(cardinalityOfIndexedFormulaExpression, quantification, process);
				System.out.println("Problem                  : " + cardinalityOfIndexedFormulaExpression);
				System.out.println("Constraint               : " + process.getContextualConstraint());
				System.out.println("Solution                 : " + result);
			}
		} 
		else if (GrinderUtil.usePlain) {
			result = usePlainCardinality(expression, process);
		}
		else {
			System.out.println("Problem: " + expression);
			Expression contextualConstraint = process.getContextualConstraint();
			System.out.println("Constraint: " + contextualConstraint);
		
			expression = CardinalityUtil.removeIfThenElsesFromFormula(expression, process);

			// Assert input arguments, | F |_x
			// 
			CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(expression);
			
			if (Sets.isEmptySet(expression.get(0))) {
				result = Expressions.ZERO;
			}
			else {
				// | {(on x1,..., xn) (x1, ..., xn) | F} |
				Expression       intensionalSet   = expression.get(0);
				Expression       f                = ((IntensionalSet) intensionalSet).getCondition();
				IndexExpressionsSet indexExpressions = ((IntensionalSet) intensionalSet).getIndexExpressions();
				
				Trace.log("F <- R_top_simplify(F)");
				f = process.rewrite(CardinalityRewriter.R_top_simplify, f);
			
				if (indexExpressions.size() == 0) {
					Trace.log("if n = 0");
					Trace.log("    return R_normalize(if F then 1 else 0)");
					Expression ifThenElse = IfThenElse.make(f, Expressions.ONE, Expressions.ZERO);
					result = process.rewrite(CardinalityRewriter.R_normalize, ifThenElse);
				} 
				else {
					Trace.log("if n > 0");
					// Note: the input f may have been simplified.
					if ( negationHasLessNumberOfDisjuncts(f) ) {
						Trace.log("    if negationHasLessNumberOfDisjuncts(F):");
						Trace.log("        return R_normalize(||X|| - R_card( | not F |_X, \"none\" ) ) ");
						Expression[] indexExpressionsAsArray = indexExpressions.toArray(new Expression[indexExpressions.size()]);
						Expression cardIndexX          = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressions);
						Expression negationCardinality = process.rewrite(R_card,
									CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
										CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(Not.make(f), indexExpressionsAsArray), 
										CardinalityRewriter.Quantification.NONE));
						Expression subtraction = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.MINUS, cardIndexX, negationCardinality);
						result = process.rewrite(CardinalityRewriter.R_normalize, subtraction);
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
			System.out.println("Solution: " + result + "\n");
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
		Expression       intensionalSet   = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f                = ((IntensionalSet) intensionalSet).getCondition();
		IndexExpressionsSet indexExpressions = ((IntensionalSet) intensionalSet).getIndexExpressions();
		List<Expression> indices          = IndexExpressions.getIndices(indexExpressions);
		Expression[]     indexExpressionsAsArray = indexExpressions.toArray(new Expression[indexExpressions.size()]);

		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol: " + quantification);
		}	
		
		if (f.equals(Expressions.TRUE)) {
			Trace.log("if F is True");
			Trace.log("    return R_normalize(||X||)");
			Expression cardIndices = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressions);
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
			result = subProcess.rewrite(CardinalityRewriter.R_normalize, cardIndices);
		}
		else if (f.equals(Expressions.FALSE)) {
			Trace.log("if F is False");
			Trace.log("    return 0");
			result = Expressions.ZERO;
		}
		else if (Expressions.expressionsDoNotOccurInAnotherExpressionAsFreeVariables(indices, f, process)) {
			Trace.log("if x does not occur in F for any x in X");
			Trace.log("    return R_normalize(if F then ||X|| else 0)");
			Expression cardIndexX = CardinalityUtil.makeCardinalityOfIndexExpressions(indexExpressions);
			Expression ifThenElse = IfThenElse.make(f, cardIndexX, Expressions.ZERO);
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
			result = subProcess.rewrite(CardinalityRewriter.R_normalize, ifThenElse);
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
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
			Expression fPrime = subProcess.rewrite(CardinalityRewriter.R_top_simplify, process.rewrite(CardinalityRewriter.R_move_not_in, f));
			Trace.log("    return R_card(|F'|_X, quantification)");
			Expression cardFPrimeIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(fPrime, indexExpressionsAsArray);
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
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
			Expression quantifierEliminated               = subProcess.rewrite(CardinalityRewriter.R_top_quantifier_elimination, f);
			Expression cardQuantifierEliminatedIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(quantifierEliminated, indexExpressionsAsArray);
			
			result = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
								cardQuantifierEliminatedIndexedByX, quantification));
		}
// Including if then else in case lists is the cleaner approach, but will take some work;
// Right now we are taking the easier approach to convert them all upfront to disjunctions.
//		else if (IfThenElse.isIfThenElse(f)) {
//			Trace.log("if F is 'if C then F1 else F2'");
//			Trace.log("    return R_card( | C and F1 or not C and F2 |_X, quantification)");
//			Expression c  = IfThenElse.getCondition(f);
//			Expression f1 = IfThenElse.getThenBranch(f);
//			Expression f2 = IfThenElse.getElseBranch(f);
//			Expression formulaWithoutIfThenElse = Or.make(And.make(c, f1), And.make(Not.make(c), f2));
//			Expression cardOfFormulaWithoutIfThenElseIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(formulaWithoutIfThenElse, indicesAsArray);
//			
//			result = process.rewrite(R_card,
//						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(
//								cardOfFormulaWithoutIfThenElseIndexedByX, quantification));
//		}
		else {
			throw new IllegalArgumentException("F is unhandled: " + f);
		}
		
		return result;
	}
}
