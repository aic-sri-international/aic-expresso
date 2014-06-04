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
package com.sri.ai.grinder.helper;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.FunctionApplicationContainsArgumentSatisfying;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalizationHierarchical;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierElimination;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.And;
import com.sri.ai.util.base.NotEquals;
import com.sri.ai.util.base.Pair;

/**
 * A library for computing conditions in a formula for determining the truth value of another condition in the same formula. 
 * 
 * @author braz
 */
@Beta
public class GetConditionsFor {

	/**
	 * Receives a formula and a variable of interest 
	 * and returns a if-then-else decision tree expression equivalent to the given formula,
	 * such that only the leaves may contain literals with the variable of interest,
	 * and such that the inner conditions contain only atoms with the other variables.
	 * This is useful for telling whether (and if so, to what) the value of a variable is implied to by the formula,
	 * under any assignment to the other variables.
	 */
	public static Expression getConditionsForVariableDPLL(Expression variable, Expression formula, RewritingProcess process) {
		formula = process.rewrite(CardinalityRewriter.R_quantifier_elimination, formula);

		Predicate<Expression> isNotGivenVariable = NotEquals.make(variable);
		Predicate<Expression> isAnotherVariable = And.make(new IsVariable(process), isNotGivenVariable);
		Predicate<Expression> containsAnotherVariable = new FunctionApplicationContainsArgumentSatisfying(isAnotherVariable);

		Expression result = conditionQuantifierFreeFormula(formula, containsAnotherVariable, process);
		
		return result;
	}

	/**
	 * Receives a quantifier-free formula and returns an equivalent decision tree (nested if then elses)
	 * such that only a given type of atom is used for conditioning (that is, to be in the inner conditions of the tree).
	 * The leaves are guaranteed to be reachable, and to not contain conditioning atoms. 
	 * This is useful for determining the truth value of a group of atoms as a function of the truth values of the remaining ones.
	 */
	public static Expression conditionQuantifierFreeFormula(Expression formula, Predicate<Expression> isConditioningAtom, RewritingProcess process) {
		Expression result;
		
		Expression splitter = FormulaUtil.pickAtomOfInterestFromQuantifierFreeFormula(formula, isConditioningAtom, process);
		
		if (splitter != null) {
			Expression splitterIsTrue  = conditionUnder(         splitter,  formula, isConditioningAtom, process);
			Expression splitterIsFalse = conditionUnder(Not.make(splitter), formula, isConditioningAtom, process);

			result = IfThenElse.make(splitter, splitterIsTrue, splitterIsFalse, false /* do not allow simplification to 'splitter', lest conditioning atom end up in leave */);
		}
		else {
			result = process.rewrite(CardinalityRewriter.R_complete_normalize, formula);			
		}
		
		return result;
	}

	private static Expression conditionUnder(Expression atomOfNoInterestIsTrue, Expression formula, Predicate<Expression> isAtomOfInterest, RewritingProcess process) {
		RewritingProcess subProcessAtomIsTrue  = GrinderUtil.extendContextualConstraint(atomOfNoInterestIsTrue,  process);
		Expression thenBranch =  subProcessAtomIsTrue.rewrite(CardinalityRewriter.R_complete_normalize, formula);
		thenBranch = conditionQuantifierFreeFormula(thenBranch, isAtomOfInterest, subProcessAtomIsTrue);
		return thenBranch;
	}

	// NOTE:
	// The implementation below is quite subtle and verbose, and depends on simplifiers to provide special guarantees.
	// Just after finishing it I understood that a much simpler approach would be to successively condition on literals of no interest
	// until only the literals of interest remained, and build a decision tree based on those conditioning, simplifying as we go.
	// It would require no special guarantees and would be much simpler than this, so it is worth doing.
	
	/**
	 * Receives a formula and a variable of interest 
	 * and returns a if-then-else decision tree expression equivalent to the given formula,
	 * and such that only the leaves contain literals with the variable of interest,
	 * and such that only the inner conditions contain literals with the other variables.
	 * This is useful for telling whether (and if so, to what) the value of a variable is implied to by the formula,
	 * under any assignment to the other variables.
	 * The tree is simplified for simple cases but not all leaves are guaranteed to be reachable.
	 */
	public static Expression getConditionsForVariable(final Expression variable, final Expression formula, final RewritingProcess process) {
		Function<Expression, Pair<Expression, Expression>> literalSeparator = new Function<Expression, Pair<Expression, Expression>>() {
			@Override
			public Pair<Expression, Expression> apply(Expression literal) {
				Pair<Expression, Expression> result = Equality.separateVariableLiteral(variable, literal, process);
				return result;
			}
		};
		Expression result = getConditionsForLiteralsOfInterest(formula, literalSeparator, process);
		return result;
	}

	/**
	 * Receives a formula and a "literal separator" that splits a literal into two equivalent literals of two different types S1 and S2
	 * (the meaning of these classes is up to the user),
	 * and returns a if-then-else decision tree expression equivalent to the given formula,
	 * and such that the only the leaves contain literals in S1,
	 * and such that only inner conditions contain literals in S2.
	 * This is useful for isolating logical variables, or propositions, of interest in the leaves so that
	 * we can obtain conditions on the *other* variables or propositions that determine a particular assignment to
	 * the variable or proposition of interest.
	 * The case with logical variables and equalities is illustrated by {@link #getConditionsForVariable(Expression, Expression, RewritingProcess)}.
	 * The tree is simplified for simple cases but not all leaves are guaranteed to be reachable.
	 */
	public static Expression getConditionsForLiteralsOfInterest(
			final Expression formula,
			final Function<Expression, Pair<Expression, Expression>> literalSeparator,
			final RewritingProcess process) {
		
		final Rewriter quantifierElimination     = new QuantifierElimination();
		final Rewriter ifThenElseExternalization = new IfThenElseExternalizationHierarchical(true, true); // preserve if-then-else structure
		Function<Expression, Expression> recursiveCall = new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression formula) {
				Expression result = getConditionsForLiteralsOfInterest(formula, literalSeparator, quantifierElimination, ifThenElseExternalization, this, process);
				return result;
			}
		};
		Expression result = recursiveCall.apply(formula);
		return result;
	}

	/**
	 * Recursive call version of {@link #getConditionsForLiteralsOfInterest(Expression, Function, RewritingProcess) receiving already-constructed intermediary objects.
	 */
	private static Expression getConditionsForLiteralsOfInterest(
			Expression formula,
			Function<Expression, Pair<Expression, Expression>> literalSeparator,
			Rewriter quantifierElimination,
			Rewriter ifThenElseExternalization,
			Function<Expression, Expression> recursiveCall,
			RewritingProcess process) {
		
		Expression result;
		if (formula.equals(Expressions.TRUE) || formula.equals(Expressions.FALSE)) {
			result = formula;
		}
		else if (ThereExists.isThereExists(formula) || ForAll.isForAll(formula)) {
			Expression quantifierFree = process.rewrite(quantifierElimination, formula);
			result = recursiveCall.apply(quantifierFree);
		}
		else if (IfThenElse.isIfThenElse(formula)) {
			Expression equivalent = IfThenElse.makeBooleanFormulaEquivalentToIfThenElse(formula);
			result = recursiveCall.apply(equivalent);
		}
		else if (formula.hasFunctor(FunctorConstants.NOT)         ||
				 formula.hasFunctor(FunctorConstants.AND)         || formula.hasFunctor(FunctorConstants.OR)          ||
				 formula.hasFunctor(FunctorConstants.IMPLIED)     || formula.hasFunctor(FunctorConstants.IMPLICATION) ||
				 formula.hasFunctor(FunctorConstants.EQUIVALENCE)) {
			List<Expression> subConditions = Util.mapIntoList(formula.getArguments(), recursiveCall);
			Expression applicationOfSameFunctionOnSubConditions = Expressions.apply(formula.getFunctor(), subConditions);
			result = process.rewrite(ifThenElseExternalization, applicationOfSameFunctionOnSubConditions);
		}
		else {
			// must be a literal
			Pair<Expression, Expression> literalOfInterestAndOther = literalSeparator.apply(formula);
			Expression literalOfInterest = literalOfInterestAndOther.first;
			Expression otherLiterals     = literalOfInterestAndOther.second;
			
			if (otherLiterals.equals(Expressions.TRUE)) {
				result = literalOfInterest;
			}
			else if (otherLiterals.equals(Expressions.FALSE)) {
				result = Expressions.FALSE;
			}
			else {
				result = Expressions.apply(FunctorConstants.IF_THEN_ELSE, otherLiterals, literalOfInterest, Expressions.FALSE);
				// it is important to use Expressions.apply instead of IfThenElse.make because the latter may simplify the expression
				// and that might cause the other variables to be in the leaves.
			}
		}
		
		// At this point, some leaves may have been simplified away and literals of no interest may be sitting in them.
		// This call makes them if then elses on "true" and "false" branches, which are then new leaves without literals of no interest.
		result = makeSureLeavesContainLiteralsOfInterestOnly(result, literalSeparator);
		
		return result;
	}

	private static Expression makeSureLeavesContainLiteralsOfInterestOnly(Expression expression, Function<Expression, Pair<Expression, Expression>> literalSeparator) {
		if (IfThenElse.isIfThenElse(expression)) {
			Expression thenBranch = IfThenElse.getThenBranch(expression);
			Expression newThenBranch = makeSureLeavesContainLiteralsOfInterestOnly(thenBranch, literalSeparator);
			Expression elseBranch = IfThenElse.getElseBranch(expression);
			Expression newElseBranch = makeSureLeavesContainLiteralsOfInterestOnly(elseBranch, literalSeparator);
			if (newThenBranch != thenBranch || newElseBranch != elseBranch) {
				expression = IfThenElse.make(IfThenElse.getCondition(expression), newThenBranch, newElseBranch);
			}
		}
		else { // this is a leaf
			if (notAndOrFormulaContainsLiteralOfNoInterest(expression, literalSeparator)) {
				expression = IfThenElse.make(expression, Expressions.TRUE, Expressions.FALSE, false);
			}
		}
		return expression;
	}

	/**
	 * Determines whether a formula with only not, and and or operators (other than non-formula atoms)
	 * contains a literal of *no* interest according to literal separator.
	 */
	private static boolean notAndOrFormulaContainsLiteralOfNoInterest(
			Expression formula,
			final Function<Expression, Pair<Expression, Expression>> literalSeparator) {
		
		boolean result;
		if (formula.equals(Expressions.TRUE) || formula.equals(Expressions.FALSE)) {
			result = false;
		}
		else if (formula.hasFunctor(FunctorConstants.NOT)         ||
				 formula.hasFunctor(FunctorConstants.AND)         || formula.hasFunctor(FunctorConstants.OR)) {
			result = Util.thereExists(formula.getArguments(), new Predicate<Expression>() {
				@Override
				public boolean apply(Expression input) {
					return notAndOrFormulaContainsLiteralOfNoInterest(input, literalSeparator);
				}} );
		}
		else {
			// must be a literal
			Pair<Expression, Expression> literalOfInterestAndOther = literalSeparator.apply(formula);
			Expression literalOfNoInterest = literalOfInterestAndOther.second;
			result = ! literalOfNoInterest.equals(Expressions.TRUE);
		}
		
		return result;
	}

}
