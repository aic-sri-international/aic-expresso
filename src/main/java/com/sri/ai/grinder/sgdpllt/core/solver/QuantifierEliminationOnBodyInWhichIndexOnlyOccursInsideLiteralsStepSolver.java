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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.util.Util.thereExists;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

/**
 * A {@link AbstractQuantifierEliminationStepSolver} for quantifiers based on a group, and with body in which the index occurs in literals only.
 * This step solver first provides all the literals in the body as {@link ExpressionStepSolver#ItDepends} steps.
 * If at any point the constraint becomes unsatisfiable, the group's identity element is returned
 * (the above is all done by {@link AbstractQuantifierEliminationStepSolver}.
 * If we reach a point in which there are no further undefined literals in the body and the constraint is satisfiable,
 * one of two things happens:
 * <ul>
 * <li> if the group is idempotent, simplify group's identity.
 * <li> if the group is not idempotent, 
 * applies {@link AssociativeCommutativeGroup#addNTimes(Expression, Expression, Context)} to
 * the literal-free body and {@link SingleVariableConstraint#modelCount(Context)},
 * followed by {@link Theory#simplify(Expression, Context)}.
 * </ul>
 * 
 * @author braz
 *
 */
@Beta
public class QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver extends AbstractQuantifierEliminationStepSolver {

	public QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint indexConstraint, Expression body) {
		super(group, indexConstraint, body);
	}

	@Override
	protected
	QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver
	makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		
		QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver result = 
				(QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver)
				clone();
		result.indexConstraint = newIndexConstraint;
		return result;
	}

	@Override
	protected Step eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			SingleVariableConstraint indexConstraint, Expression literalFreeBody, Context context)
		throws IndexOccursInBodyException {
		
		Expression result;
		
		Expression index = indexConstraint.getVariable();
		SubExpressionsDepthFirstIterator allSubExpressions = new SubExpressionsDepthFirstIterator(literalFreeBody);
		
		if (thereExists(allSubExpressions, s -> s.equals(index))) {
			throwExceptionIndicatingIndexIsInBodyButNotInLiteral(index, indexConstraint, literalFreeBody, context);
			result = null; // never runs; making compiler happy about uninitialized 'result'
		}
		else if (getGroup().isIdempotent()) {
			result = resultForIdempotentGroup(index, indexConstraint, literalFreeBody, context);
		}
		else {
			result = resultIsBodyTimesNumberOfIndexValues(indexConstraint, literalFreeBody, context);
		}
		
		return new Solution(result);
	}

	public static class IndexOccursInBodyException extends IllegalArgumentException {
		private static final long serialVersionUID = 1L;
		public IndexOccursInBodyException(String message) {
			super(message);
		}
	}
	
	private void throwExceptionIndicatingIndexIsInBodyButNotInLiteral(
			Expression index, 
			SingleVariableConstraint indexConstraint, 
			Expression literalFreeBody, 
			Context context)
		throws IndexOccursInBodyException {
		
		Expression typeExpressionOfIndex = context.getTypeExpressionOfRegisteredSymbol(index);
		Expression problemExpression = group.makeProblemExpression(index, typeExpressionOfIndex, indexConstraint, literalFreeBody);
		throw new IndexOccursInBodyException(
				QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver.class +
				": Index occurs in body: " + problemExpression);
	}

	private Expression resultForIdempotentGroup(Expression index, SingleVariableConstraint indexConstraint, Expression literalFreeBody, Context context) throws Error {
		Expression result;
		Expression conditionForSatisfiability = indexConstraint.satisfiability(context);
		if (conditionForSatisfiability == null) {
			throw new Error("No satisfiability solver present for " + index + ". Need to implement re-construction of original expression");
		}
		result = IfThenElse.makeWithoutConditionalCondition(conditionForSatisfiability, literalFreeBody, getGroup().additiveIdentityElement());
		return result;
	}

	private Expression resultIsBodyTimesNumberOfIndexValues(SingleVariableConstraint indexConstraint, Expression literalFreeBody, Context context) {
		Expression result;
		result = getGroup().addNTimes(literalFreeBody, indexConstraint.modelCount(context), context);
		result = context.getTheory().simplify(result, context);
		return result;
	}
}