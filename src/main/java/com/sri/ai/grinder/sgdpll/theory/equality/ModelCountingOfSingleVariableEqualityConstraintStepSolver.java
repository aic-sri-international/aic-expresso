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
package com.sri.ai.grinder.sgdpll.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static java.lang.Math.max;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.core.solver.AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver;

/**
 * A {@link AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver}
 * for a {@link SingleVariableEqualityConstraint}.
 * As such, it provides a way of computing a solution when all propagated literals and propagated CNF are satisfied
 * by the context.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableEqualityConstraintStepSolver extends AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver {

	private NumberOfDistinctExpressionsStepSolver numberOfDistinctExpressionsStepSolver;
	
	public ModelCountingOfSingleVariableEqualityConstraintStepSolver(SingleVariableEqualityConstraint constraint) {
		super(constraint);
		numberOfDistinctExpressionsStepSolver = new NumberOfDistinctExpressionsStepSolver(getConstraint().getDisequals());
	}

	@Override
	public ModelCountingOfSingleVariableEqualityConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableEqualityConstraintStepSolver) super.clone();
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) super.getConstraint();
	}
	
	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(
			Constraint contextualConstraint, Context context) {

		Expression solutionExpression;
		if (getConstraint().getEqualsIterator().hasNext()) { // variable is bound to some value
			solutionExpression = ONE;
		}
		else {
			SolutionStep step = numberOfDistinctExpressionsStepSolver.step(contextualConstraint, context);
			if (step.itDepends()) {
				ModelCountingOfSingleVariableEqualityConstraintStepSolver ifTrue = clone();
				ifTrue.numberOfDistinctExpressionsStepSolver = (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenLiteralIsTrue();
				ModelCountingOfSingleVariableEqualityConstraintStepSolver ifFalse = clone();
				ifFalse.numberOfDistinctExpressionsStepSolver = (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenLiteralIsFalse();
				SolutionStep result = new ItDependsOn(step.getLiteral(), step.getConstraintSplitting(), ifTrue, ifFalse);
				return result;
			}
			long numberOfNonAvailableValues = step.getValue().longValue();

			long variableDomainSize = getConstraint().getVariableTypeSize(context);
			if (variableDomainSize == -1) {
				Expression variableDomain = getConstraint().getVariableTypeExpression(context);
				Expression variableDomainCardinality = apply(CARDINALITY, variableDomain);
				solutionExpression = Minus.make(variableDomainCardinality, makeSymbol(numberOfNonAvailableValues));
			}
			else if (variableDomainSize == -2) {
				solutionExpression = makeSymbol("infinity");
			}
			else {
				solutionExpression = makeSymbol(max(0, variableDomainSize - numberOfNonAvailableValues));
			}
		}
		return new Solution(solutionExpression);
	}
}