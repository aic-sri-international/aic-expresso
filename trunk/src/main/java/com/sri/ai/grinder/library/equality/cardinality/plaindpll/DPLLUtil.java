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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.freeVariablesAndTypes;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndexExpressionsFromSymbolsAndTypes;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * Implements utility methods to be used by {@link DPLLGeneralizedAndSymbolic} and associated classes.
 * <p>
 * Several of these methods could be more naturally seen as methods of the interfaces themselves
 * (for example, {@link DPLLUtil#getIndexBoundBySplitterApplicationIfAny(Expression splitter, Collection<Expression> indices, Theory theory)}
 * could be a method in interface {@link Theory}),
 * but are included here instead because their functionality depends on a more basic method of those interfaces
 * (in that case, on {@link Theory#makeConstraint()});
 * including them there would place the burden on users to make sure the implementations of these multiple methods are mutually consistent.
 * Default interface methods would not be a good solution either because the burden would still be there for the user not to override
 * the default method, or if they did, to make sure they are consistent (there are no "final default" interface methods).
 * By placing these methods here, users implementing an interface can more easily simply implement each method as a primitive
 * consistent with its pre- and post-conditions only, and have those primitive methods be automatically used by the convenience ones here.
 *   
 * @author braz
 *
 */
@Beta
public class DPLLUtil {

	/**
	 * Simplifies the top expression of an equality-logic-with-quantifiers formula until it cannot be simplified anymore.
	 * Always returns either a symbol or a function application (quantified formulas have their top quantifiers eliminated).
	 * @param expression
	 * @param functionApplicationSimplifiers TODO
	 * @param syntacticFormTypeSimplifiers TODO
	 * @param process
	 * @return
	 */
	protected static Expression topSimplifyExhaustively(Expression expression, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers, RewritingProcess process) {
		
		Expression previous;
		do {
			expression = topSimplifyOnce(previous = expression, functionApplicationSimplifiers, syntacticFormTypeSimplifiers, process);
		} while (expression != previous);
		
		return expression;
	}

	private static Expression topSimplifyOnce(Expression expression, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers, RewritingProcess process) {
		BinaryFunction<Expression, RewritingProcess, Expression> simplifier;
		if (expression.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			simplifier = functionApplicationSimplifiers.get(expression.getFunctor().getValue());
		}
		else {
			simplifier = syntacticFormTypeSimplifiers.get(expression.getSyntacticFormType());
		}
		
		if (simplifier != null) {
			expression = simplifier.apply(expression, process);
		}
		
		return expression;
	}

	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with given top simplifier, then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	protected static Expression simplify(
			Expression expression,
			BinaryFunction<Expression, RewritingProcess, Expression> topSimplifier,
			RewritingProcess process) {
		
		Expression result = expression;
		result = topSimplifier.apply(result, process);
		if (result.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			List<Expression> originalArguments = result.getArguments();
			ArrayList<Expression> simplifiedArguments =
					Util.mapIntoArrayList(originalArguments, e -> simplify(e, topSimplifier, process));
			if ( ! Util.sameInstancesInSameIterableOrder(originalArguments, simplifiedArguments)) { // this check speeds cardinality algorithm by about 25%; it is also required for correctness wrt not returning a new instance that is equal to the input.
				result = Expressions.apply(result.getFunctor(), simplifiedArguments);
			}
			result = topSimplifier.apply(result, process);
		}
	
		return result;
	}

	/**
	 * A method provided for use with {@link Rewriter} code using contextual constraints.
	 * Will probably be discarded at some point.
	 * @param solution
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression simplifySolutionUnderConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (constraint.equals(Expressions.TRUE)) {
			result = solution;
		}
		else {
			result = simplifySolutionUnderNonTrivialConstraint(solution, constraint, process);
		}
	
		return result;
	}

	/**
	 * A method provided for use with {@link Rewriter} code using contextual constraints.
	 * Will probably be discarded at some point.
	 * @param solution
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression simplifySolutionUnderNonTrivialConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (IfThenElse.isIfThenElse(solution)) {
			Expression newCondition = impliesExpressionOrItsNegationOrNeither(IfThenElse.getCondition(solution), constraint, process);
			if (newCondition.equals(Expressions.TRUE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getThenBranch(solution), constraint, process);
			}
			else if (newCondition.equals(Expressions.FALSE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getElseBranch(solution), constraint, process);
			}
			else {
				Expression newThenBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getThenBranch(solution), constraint, process);
				Expression newElseBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getElseBranch(solution), constraint, process);
				result = IfThenElse.makeIfDistinctFrom(solution, newCondition, newThenBranch, newElseBranch, false);
			}
		}
		else {
			result = solution;
		}
		return result;
	}

	/**
	 * Returns 'true' if expression is tautologically implied by constraint,
	 * 'false' if its negation is tautologically implied by constraint,
	 * and expression itself otherwise.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression impliesExpressionOrItsNegationOrNeither(Expression expression, Expression constraint, RewritingProcess process) {
		Expression result = null;
	
		Expression constraintImpliesExpression = Implication.make(constraint, expression);
		List<Expression> freeVariablesIndexExpressions = getIndexExpressionsFromSymbolsAndTypes(freeVariablesAndTypes(constraintImpliesExpression, process));
	
		Expression closedConstraintImpliedExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesExpression);
		Expression alwaysImpliesExpression = (new SymbolEqualityTautologicalityDPLL()).rewrite(closedConstraintImpliedExpression, process);
		if (alwaysImpliesExpression.equals(Expressions.TRUE)) {
			result = Expressions.TRUE;
		}
		else {
			Expression constraintImpliesNegationOfExpression = Implication.make(constraint, Not.make(expression));
			Expression closedConstraintImpliesNegationOfExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesNegationOfExpression);
			Expression alwaysImpliesNegationOfExpression = (new SymbolEqualityTautologicalityDPLL()).rewrite(closedConstraintImpliesNegationOfExpression, process);
			if (alwaysImpliesNegationOfExpression.equals(Expressions.TRUE)) {
				result = Expressions.FALSE;
			}
			else {
				result = expression;
			}
		}
	
		return result;
	}

	/**
	 * @param solution
	 * @param process
	 * @return
	 */
	public static boolean isConditionalSolution(Expression solution, Theory theory, RewritingProcess process) {
		boolean result = IfThenElse.isIfThenElse(solution) && isSplitter(IfThenElse.getCondition(solution), theory, process);
		return result;
	}

	public static boolean isSplitter(Expression literal, Theory theory, RewritingProcess process) {
		boolean result = theory.makeSplitterIfPossible(literal, Util.list(), process) != null;
		return result;
	}
}