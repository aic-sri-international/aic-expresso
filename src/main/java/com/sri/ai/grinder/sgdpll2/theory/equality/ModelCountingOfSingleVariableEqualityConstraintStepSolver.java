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
package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.list;
import static java.lang.Math.max;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityAndDefiningLiteralsStepSolver;

/**
 * A {@link AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityAndDefiningLiteralsStepSolver}
 * for a {@link SingleVariableEqualityConstraint}.
 * As such, it provides defining literals (propagated literals are delegated, by the super class, to a satisfiability step solver)
 * and to provide a way of computing a solution when all propagated literals and propagated CNF are satisfied,
 * and all defining literals are defined by the context.
 * <p>
 * As established at the {@link AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver},
 * defining literals are literals on which satisfiability does not depend, but the model count does.
 * <p>
 * For equality theory, defining literals are none if the constraint's variable is already bound to some value.
 * In this case, the model count is, naturally, <code>1</code>.
 * Otherwise, the defining literals are all equalities among "disequals" of the constraint's variable otherwise, where
 * "disequals" are the values to which the variable is constrained to be disequal from.
 * For example, <code>X != Y and X != Z</code> is <code>|type(X)| - 1</code> if <code>Y = Z</code>,
 * and <code>2</code> otherwise.
 * <p>
 * Once all such equalities have been decided, the solver simply returns the solution
 * <code>|type(X)| - |unique disequals|</code>
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableEqualityConstraintStepSolver extends AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityAndDefiningLiteralsStepSolver {

	private NumberOfDistinctExpressionsStepSolver numberOfDistinctExpressionsStepSolver;
	
	public ModelCountingOfSingleVariableEqualityConstraintStepSolver(SingleVariableEqualityConstraint constraint) {
		super(constraint);
		numberOfDistinctExpressionsStepSolver = new NumberOfDistinctExpressionsStepSolver(getConstraint().getDisequals());
	}

	public ModelCountingOfSingleVariableEqualityConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableEqualityConstraintStepSolver) super.clone();
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) super.getConstraint();
	}
	
	@Override
	protected Iterable<Expression> getDefiningLiterals(Constraint2 contextualConstraint, RewritingProcess process) {
		return list();
	}

	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(
			Constraint2 contextualConstraint, RewritingProcess process) {

		Expression solutionExpression;
		if (getConstraint().getEqualsIterator().hasNext()) { // variable is bound to some value
			solutionExpression = ONE;
		}
		else {
			SolutionStep step = numberOfDistinctExpressionsStepSolver.step(contextualConstraint, process);
			if (step.itDepends()) {
				ModelCountingOfSingleVariableEqualityConstraintStepSolver ifTrue = clone();
				ifTrue.numberOfDistinctExpressionsStepSolver = (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenExpressionIsTrue();
				ModelCountingOfSingleVariableEqualityConstraintStepSolver ifFalse = clone();
				ifFalse.numberOfDistinctExpressionsStepSolver = (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenExpressionIsFalse();
				SolutionStep result = new ItDependsOn(step.getExpression(), step.getConstraintSplitting(), ifTrue, ifFalse);
				return result;
			}
			long numberOfNonAvailableValues = step.getExpression().longValue();

			long variableDomainSize = getConstraint().getVariableTypeSize(process);
			if (variableDomainSize == -1) {
				Expression variableDomain = getConstraint().getVariableTypeExpression(process);
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