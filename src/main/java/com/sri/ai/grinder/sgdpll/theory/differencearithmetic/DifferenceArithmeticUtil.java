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
package com.sri.ai.grinder.sgdpll.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.util.collect.NestedIterator;

/**
 * A collection of methods for manipulating difference arithmetic literals.
 *
 * @author braz
 *
 */
@Beta
public class DifferenceArithmeticUtil {

	/** 
	 * A triple containing the components of a difference arithmetic literal:
	 * the positive, negative and constant terms in it.
	 * @author braz
	 *
	 */
	private static class DAParts {
		private Set<Expression> positives;
		private Set<Expression> negatives;
		private int constant;
		
		public DAParts() {
			this(new LinkedHashSet<>(), new LinkedHashSet<>(), 0);
		}
		
		public DAParts(Set<Expression> positiveTerms, Set<Expression> negativeTerms, int constant) {
			this.positives = positiveTerms;
			this.negatives = negativeTerms;
			this.constant  = constant;
		}
	}
	
	/**
	 * Simplify a difference arithmetic literal.
	 * @param expression
	 * @param constraintTheory TODO
	 * @param context TODO
	 * @return
	 */
	public static Expression simplify(Expression expression, DifferenceArithmeticConstraintTheory constraintTheory, Context context) {
		if (! constraintTheory.isLiteral(expression, context)) {
			return expression;
		}
		DAParts parts = makeDifferenceArithmeticTriple(expression);
		ArrayList<Expression> leftHandSideArguments  = new ArrayList<Expression>(parts.positives);
		ArrayList<Expression> rightHandSideArguments = new ArrayList<Expression>(parts.negatives); // negatives in the left-hand side (all elements in parts are supposed to be there) move to right-hand side as positives
		if (parts.constant >= 0) {
			leftHandSideArguments.add(makeSymbol(parts.constant));
		}
		else {
			rightHandSideArguments.add(makeSymbol(-parts.constant));
		}
		Expression result = Expressions.apply(expression.getFunctor(), Plus.make(leftHandSideArguments), Plus.make(rightHandSideArguments));
		if (result.equals(expression)) {
			result = expression; // make sure to return the same instance if there has been no change, as simplifiers rely on that to know something didn't change
		}
		return result;
	}

	/**
	 * Returns an expression equivalent to difference arithmetic expression <code>numericalComparison</code> in which terms are cancelled out,
	 * numerical constants are summed together, and the given variable occurs alone in one of the sides.
	 * Terms on the side opposite to the variable are sorted according to their natural order.
	 * @param variable
	 * @param numericalComparison
	 * @return
	 * @throws Error
	 */
	public static Expression isolateVariable(Expression variable, Expression numericalComparison) throws Error {
		Expression result;

		DAParts parts = makeDifferenceArithmeticTriple(numericalComparison);

		Set<Expression> positiveVariables = parts.positives;
		Set<Expression> negativeVariables = parts.negatives;
		int constant = parts.constant;

		// now isolate variable:

		// create array for all the arguments of the sum that is going to be on the side opposite to the variable
		ArrayList<Expression> sumArguments = new ArrayList<Expression>(positiveVariables.size() + negativeVariables.size() -1 + 1); // minus variable, plus constant
		if (positiveVariables.contains(variable)) {
			for (Expression positiveVariable : positiveVariables) { // variable will be on left-hand side: invert signs of everybody else (negative variables become positive)
				if ( ! positiveVariable.equals(variable)) {
					sumArguments.add(Expressions.apply(MINUS, positiveVariable));
				}
			}
			for (Expression negativeVariable : negativeVariables) {
				sumArguments.add(negativeVariable);
			}
			sumArguments.add(makeSymbol(-constant));

			if (positiveVariables.contains(variable)) { // check again, since variable may have been canceled out
				Expression oppositeSide = Plus.make(sumArguments);
				result = Expressions.apply(numericalComparison.getFunctor(), variable, oppositeSide);
			}
			else {
				throw new Error("Trying to isolate " + variable + " in " + numericalComparison + " but it gets canceled out");
			}
		}
		else {
			for (Expression positiveVariable : positiveVariables) { // variable will be on right-hand side: everybody else stays on left-hand side and keeps their sign (negative variables get the negative sign in their representation)
				sumArguments.add(positiveVariable);
			}
			for (Expression negativeVariable : negativeVariables) {
				if ( ! negativeVariable.equals(variable)) {
					sumArguments.add(Expressions.apply(MINUS, negativeVariable));
				}
			}
			sumArguments.add(makeSymbol(constant));

			Collections.sort(sumArguments); // this increases readability and normalization.
			
			if (negativeVariables.contains(variable)) { // check again, since variable may have been canceled out
				Expression oppositeSide = Plus.make(sumArguments);
				result = Expressions.apply(numericalComparison.getFunctor(), oppositeSide, variable);
			}
			else {
				throw new Error("Trying to isolate " + variable + " in " + numericalComparison + " but it gets canceled out");
			}
		}
		
		if (result.equals(numericalComparison)) {
			result = numericalComparison; // make sure to return the same instance if there has been no change, as simplifiers rely on that to know something didn't change
		}

		return result;
	}

	/**
	 * Given a numerical comparison expression,
	 * provides a triple with the terms with positive and negative signs, as well as an integer constant,
	 * describing the situation in which they are all moved to the left-hand side
	 * terms with opposite signs are canceled out, and all integer constants are summed together.
	 * If the <code>makeDuplicateError</code> function is not null, the detection of two terms with the same sign
	 * will throw the Error provided by that function.
	 * @param numericalComparison
	 * @return
	 * @throws Error
	 */
	private static DAParts makeDifferenceArithmeticTriple(Expression numericalComparison) {
		
		Function<Expression, Error> makeDuplicateError =
				duplicate -> makeDuplicateError(numericalComparison, duplicate);
		
		DAParts
		items0 = gatherPositiveAndNegativeTermsAndConstantInteger(numericalComparison.get(0), makeDuplicateError);

		DAParts
		items1 = gatherPositiveAndNegativeTermsAndConstantInteger(numericalComparison.get(1), makeDuplicateError);

		DAParts
		result = subtractDifferenceLogicTriples(numericalComparison, items0, items1, makeDuplicateError);

		return result;
	}

	/**
	 * Given a sum, or an expression to be interpreted as a single-term sum,
	 * returns a triple containing a multiset of positive terms, a multiset of negative terms,
	 * and the sum of all numerical constants in it.
	 * If <code>makeDuplicateError</code> is non-null and a duplicate term is found,
	 * the duplicate is passed to it as a parameter and the resulting Error is thrown.
	 * @param expression
	 * @param makeDuplicateError a function that, if non-null, gets a duplicate term and makes an Error to be thrown 
	 * @return
	 */
	private static DAParts 
	gatherPositiveAndNegativeTermsAndConstantInteger(
			Expression expression, 
			Function<Expression, Error> makeDuplicateError) {
		
		DAParts result = new DAParts();

		List<Expression> arguments = Plus.getSummands(expression);
		for (Expression argument : arguments) {
			if (argument.hasFunctor(MINUS) && argument.numberOfArguments() == 1) {
				argument = UnaryMinus.simplify(argument); // removes double minuses
			}
			if (argument.hasFunctor(MINUS)) {
				Expression negationArgument = argument.get(0);
				if (negationArgument.getValue() instanceof Number) {
					result.constant = result.constant - ((Number) negationArgument.getValue()).intValue(); // note the -  !
				}
				else {
					if (result.negatives.contains(negationArgument)) {
						throw makeDuplicateError.apply(negationArgument);
					}
					else if (result.positives.contains(negationArgument)) {
						result.positives.remove(negationArgument); // cancel out with the positive one, and don't add it to negatives
					}
					else {
						result.negatives.add(negationArgument);
					}
				}
			}
			else {
				if (argument.getValue() instanceof Number) {
					result.constant = result.constant + ((Number) argument.getValue()).intValue(); // note the +  !
				}
				else {
					if (result.positives.contains(argument)) {
						throw makeDuplicateError.apply(argument);
					}
					else if (result.negatives.contains(argument)) {
						result.negatives.remove(argument); // cancel out with the negative one, and don't add it to positives
					}
					else {
						result.positives.add(argument);
					}
				}
			}
		}

		return result;
	}

	/**
	 * Given two difference arithmetic tuples, each containing positive and negative terms and a numeric constant in a summation,
	 * returns another tuple of the same form representing their subtraction,
	 * or throws an Error if any of the terms appears with the same final sign multiple times
	 * (which would require representing a multiple of it), such as in ({X}, {}, 1) - ({}, {X}, 2)
	 * which would result in 2*X - 1.
	 * @param numericalComparison TODO
	 * @param positiveAndNegativeTermsAndConstant1
	 * @param positiveAndNegativeTermsAndConstant2
	 * @param makeDuplicateError a function getting the offending duplicate term and returning an Error to be thrown.
	 * @return
	 * @throws Error
	 */
	private static DAParts subtractDifferenceLogicTriples(
			Expression numericalComparison, 
			DAParts positiveAndNegativeTermsAndConstant1, 
			DAParts positiveAndNegativeTermsAndConstant2, Function<Expression, Error> makeDuplicateError) {

		// cancel out terms that are positive in both first and second parts (they cancel because second parts is being subtracted):
		Iterator<Expression> positive1Iterator = positiveAndNegativeTermsAndConstant1.positives.iterator();
		while (positive1Iterator.hasNext()) {
			Expression positive1 = positive1Iterator.next();
			if (positiveAndNegativeTermsAndConstant2.positives.contains(positive1)) {
				positive1Iterator.remove();
				positiveAndNegativeTermsAndConstant2.positives.remove(positive1);
			}
		}
		
		// cancel out terms that are negative in both first and second parts (they cancel because second parts is being subtracted):
		Iterator<Expression> negative1Iterator = positiveAndNegativeTermsAndConstant1.negatives.iterator();
		while (negative1Iterator.hasNext()) {
			Expression negative1 = negative1Iterator.next();
			if (positiveAndNegativeTermsAndConstant2.negatives.contains(negative1)) {
				negative1Iterator.remove();
				positiveAndNegativeTermsAndConstant2.negatives.remove(negative1);
			}
		}
		
		Set<Expression> unionOfPositiveTerms = new LinkedHashSet<>();
		Iterable<Expression> positiveTerms = in(new NestedIterator<Expression>(
				positiveAndNegativeTermsAndConstant1.positives,
				positiveAndNegativeTermsAndConstant2.negatives)); // negative terms in second tuple are actually positive since it is being subtracted
		
		for (Expression positive : positiveTerms) {
			boolean noDuplicate = unionOfPositiveTerms.add(positive);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw makeDuplicateError.apply(positive);
			}
			else if (unionOfPositiveTerms.size() == 2) {
				throw new Error(numericalComparison + " is not a difference arithmetic atom because it contains more than one positive term when moved to the left-hand side: " + join(unionOfPositiveTerms));
			}
		}
		
		Set<Expression> unionOfNegativeTerms = new LinkedHashSet<>();
		Iterable<Expression> negativeTerms = in(new NestedIterator<Expression>(
				positiveAndNegativeTermsAndConstant1.negatives,
				positiveAndNegativeTermsAndConstant2.positives)); // positive terms in second tuple are actually negative since it is being subtracted
		
		for (Expression negative : negativeTerms) {
			boolean noDuplicate = unionOfNegativeTerms.add(negative);
			boolean duplicate = ! noDuplicate;
			if (duplicate) {
				throw makeDuplicateError.apply(negative);
			}
			else if (unionOfNegativeTerms.size() == 2) {
				throw new Error(numericalComparison + " is not a difference arithmetic atom because it contains more than one negative term when moved to the left-hand side: " + join(unionOfNegativeTerms));
			}
		}
		
		int constant = positiveAndNegativeTermsAndConstant1.constant - positiveAndNegativeTermsAndConstant2.constant;
	
		DAParts result = new DAParts(unionOfPositiveTerms, unionOfNegativeTerms, constant);
		return result;
	}

	/**
	 * @param numericalComparison
	 * @param duplicate
	 * @return
	 */
	private static Error makeDuplicateError(Expression numericalComparison, Expression duplicate) {
		return new Error(
				numericalComparison + " is not a difference arithmetic atom because " 
						+ duplicate + " sums with itself, but no multiples are allowed in difference arithmetic");
	}
}