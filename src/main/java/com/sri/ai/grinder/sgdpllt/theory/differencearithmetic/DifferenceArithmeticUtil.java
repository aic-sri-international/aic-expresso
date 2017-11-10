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
import static com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticLiteralSide.makeDifferenceArithmeticNonZeroSideOfLiteralEquivalentTo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.number.Plus;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticLiteralSide.DifferenceArithmeticLiteralSideException;

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
		Expression result;
		
		try {
			if (theory.isLiteralOrBooleanConstant(expression, context)) {
				DifferenceArithmeticLiteralSide literalSide = makeDifferenceArithmeticNonZeroSideOfLiteralEquivalentTo(expression);
				ArrayList<Expression> leftHandSideArguments  = new ArrayList<Expression>(literalSide.getPositives());
				ArrayList<Expression> rightHandSideArguments = new ArrayList<Expression>(literalSide.getNegatives()); // negatives in the left-hand side (all elements in term are supposed to be there) move to right-hand side as positives
				if (literalSide.getConstant() >= 0) {
					leftHandSideArguments.add(makeSymbol(literalSide.getConstant()));
				}
				else {
					rightHandSideArguments.add(makeSymbol(-literalSide.getConstant()));
				}
				result = Expressions.apply(expression.getFunctor(), Plus.make(leftHandSideArguments), Plus.make(rightHandSideArguments));
				result = makeSureInstanceIsTheSameIfNotChanged(expression, result);
			}
			else {
				result = expression;
			}
		}
		catch (DifferenceArithmeticLiteralSideException exception) {
			result = expression;
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
		try {
			DifferenceArithmeticLiteralSide literalSide = makeDifferenceArithmeticNonZeroSideOfLiteralEquivalentTo(numericalComparison);
			Set<Expression> positiveVariables = literalSide.getPositives();
			Set<Expression> negativeVariables = literalSide.getNegatives();
			int constant = literalSide.getConstant();
			result = isolateDependingOnWhetherMainVariableIsPositiveOrNegative(variable, numericalComparison, positiveVariables, negativeVariables, constant);
			result = makeSureInstanceIsTheSameIfNotChanged(numericalComparison, result);
		}
		catch (DifferenceArithmeticLiteralSideException exception) {
			throw new Error("Trying to isolate " + variable + " in " + numericalComparison + " but the latter is not a valid difference arithmetic literal");
		}
		return result;
	}

	private static Expression isolateDependingOnWhetherMainVariableIsPositiveOrNegative(Expression variable, Expression numericalComparison, Set<Expression> positiveVariables, Set<Expression> negativeVariables, int constant) throws Error {
		Expression result;
		if (positiveVariables.contains(variable)) {
			result = makeComparisonBetweenAllOtherTermsAndPositiveVariableIfNotCanceledOut(variable, numericalComparison, positiveVariables, negativeVariables, constant);
		}
		else {
			result = makeComparisonBetweenAllOtherTermsAndNegativeVariableIfNotCanceledOut(variable, numericalComparison, positiveVariables, negativeVariables, constant);
		}
		return result;
	}

	private static Expression makeSureInstanceIsTheSameIfNotChanged(Expression numericalComparison, Expression result) {
		if (result.equals(numericalComparison)) {
			result = numericalComparison;
		}
		return result;
	}

	private static Expression makeComparisonBetweenAllOtherTermsAndPositiveVariableIfNotCanceledOut(Expression variable, Expression numericalComparison, Set<Expression> positiveVariables, Set<Expression> negativeVariables, int constant) throws Error {
		ArrayList<Expression> oppositeSide = makeOppositeSideBySendingAllTermsOtherThanVariableWithFlippedSign(variable, positiveVariables, negativeVariables, constant);
		Expression result = makeComparisonBetweenVariableAndOppositeSide(variable, numericalComparison, oppositeSide, positiveVariables);
		return result;
	}

	private static ArrayList<Expression> makeOppositeSideBySendingAllTermsOtherThanVariableWithFlippedSign(Expression variable, Set<Expression> positiveVariables, Set<Expression> negativeVariables, int constant) {
		ArrayList<Expression> oppositeSide = new ArrayList<Expression>(positiveVariables.size() + negativeVariables.size() -1 + 1); // minus variable, plus constant
		gatherVariablesOtherThanMainVariableWithFlippedSignInOneSide(variable, positiveVariables, oppositeSide);
		gatherVariablesWithFlippedSignInOneSide(negativeVariables, oppositeSide);
		sendConstantWithFlippedSignToSide(constant, oppositeSide);
		return oppositeSide;
	}

	private static void gatherVariablesOtherThanMainVariableWithFlippedSignInOneSide(Expression mainVariable, Set<Expression> variables, ArrayList<Expression> side) {
		for (Expression otherVariable : variables) {
			if ( ! otherVariable.equals(mainVariable)) {
				side.add(Expressions.apply(MINUS, otherVariable));
			}
		}
	}

	private static void gatherVariablesWithFlippedSignInOneSide(Set<Expression> variables, ArrayList<Expression> side) {
		for (Expression variable : variables) {
			side.add(variable);
		}
	}

	private static void sendConstantWithFlippedSignToSide(int constant, ArrayList<Expression> side) {
		side.add(makeSymbol(-constant));
	}

	private static Expression makeComparisonBetweenVariableAndOppositeSide(Expression variable, Expression numericalComparison, ArrayList<Expression> sumArguments, Set<Expression> positiveVariables) throws Error {
		Expression result;
		if (positiveVariables.contains(variable)) { // check again, since variable may have been canceled out
			Expression oppositeSide = Plus.make(sumArguments);
			result = Expressions.apply(numericalComparison.getFunctor(), variable, oppositeSide);
		}
		else {
			throw new Error("Trying to isolate " + variable + " in " + numericalComparison + " but it gets canceled out");
		}
		return result;
	}

	private static Expression makeComparisonBetweenAllOtherTermsAndNegativeVariableIfNotCanceledOut(Expression variable, Expression numericalComparison, Set<Expression> positiveVariables, Set<Expression> negativeVariables, int constant) throws Error {
		ArrayList<Expression> thisSide = makeThisSideByKeepingAllTermsOtherThanVariable(variable, positiveVariables, negativeVariables, constant);
		Expression result = makeComparisonBetweenSideAndVariableIfNotCanceledOut(thisSide, numericalComparison, variable, negativeVariables);
		return result;
	}

	private static ArrayList<Expression> makeThisSideByKeepingAllTermsOtherThanVariable(Expression variable, Set<Expression> positiveVariables, Set<Expression> negativeVariables, int constant) {
		ArrayList<Expression> thisSide = new ArrayList<Expression>(positiveVariables.size() + negativeVariables.size() -1 + 1); // minus variable, plus constant
		gatherVariablesWithFlippedSignInOneSide(positiveVariables, thisSide);
		gatherVariablesOtherThanMainVariableWithFlippedSignInOneSide(variable, negativeVariables, thisSide);
		thisSide.add(makeSymbol(constant));
		sortForIncreasedReadability(thisSide);
		return thisSide;
	}

	private static void sortForIncreasedReadability(ArrayList<Expression> sumArguments) {
		Collections.sort(sumArguments);
	}

	private static Expression makeComparisonBetweenSideAndVariableIfNotCanceledOut(ArrayList<Expression> side, Expression numericalComparison, Expression variable, Set<Expression> negativeVariables) throws Error {
		Expression result;
		if (negativeVariables.contains(variable)) { // check again, since variable may have been canceled out
			result = reassembleComparisonWithIsolatedNegativeVariable(variable, numericalComparison, side);
		}
		else {
			throw new Error("Trying to isolate " + variable + " in " + numericalComparison + " but it gets canceled out");
		}
		return result;
	}

	private static Expression reassembleComparisonWithIsolatedNegativeVariable(Expression variable, Expression numericalComparison, ArrayList<Expression> side) {
		Expression sideSummation = Plus.make(side);
		Expression result = Expressions.apply(numericalComparison.getFunctor(), sideSummation, variable);
		return result;
	}
}