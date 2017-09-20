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
package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticLiteralSide.subtractDifferenceArithmeticLiteralSides;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.number.Plus;

/**
 * A collection of methods for manipulating difference arithmetic literals.
 *
 * @author braz
 *
 */
@Beta
public class DifferenceArithmeticUtil {

	/**
	 * Simplify a difference arithmetic literal.
	 * @param expression
	 * @param theory TODO
	 * @param context TODO
	 * @return
	 */
	public static Expression simplify(Expression expression, DifferenceArithmeticTheory theory, Context context) {
		if (! theory.isLiteralOrBooleanConstant(expression, context)) {
			return expression;
		}
		DifferenceArithmeticLiteralSide term = makeDifferenceArithmeticLiteralNonZeroSide(expression);
		ArrayList<Expression> leftHandSideArguments  = new ArrayList<Expression>(term.getPositives());
		ArrayList<Expression> rightHandSideArguments = new ArrayList<Expression>(term.getNegatives()); // negatives in the left-hand side (all elements in term are supposed to be there) move to right-hand side as positives
		if (term.getConstant() >= 0) {
			leftHandSideArguments.add(makeSymbol(term.getConstant()));
		}
		else {
			rightHandSideArguments.add(makeSymbol(-term.getConstant()));
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

		DifferenceArithmeticLiteralSide term = makeDifferenceArithmeticLiteralNonZeroSide(numericalComparison);

		Set<Expression> positiveVariables = term.getPositives();
		Set<Expression> negativeVariables = term.getNegatives();
		int constant = term.getConstant();

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
	 * provides a {@link DifferenceArithmeticLiteralSide} representing the non-zero side of an equivalent
	 * difference arithmetic literal in which the other size is 0.
	 * If the <code>makeDuplicateError</code> function is not null, the detection of two terms with the same sign
	 * will throw the Error provided by that function.
	 * @param numericalComparison
	 * @return
	 * @throws Error
	 */
	private static DifferenceArithmeticLiteralSide makeDifferenceArithmeticLiteralNonZeroSide(Expression numericalComparison) {
		
		DifferenceArithmeticLiteralSide
		leftHandSide = new DifferenceArithmeticLiteralSide(numericalComparison.get(0));

		DifferenceArithmeticLiteralSide
		rightHandSide = new DifferenceArithmeticLiteralSide(numericalComparison.get(1));

		DifferenceArithmeticLiteralSide
		result = subtractDifferenceArithmeticLiteralSides(numericalComparison, leftHandSide, rightHandSide);

		return result;
	}
}