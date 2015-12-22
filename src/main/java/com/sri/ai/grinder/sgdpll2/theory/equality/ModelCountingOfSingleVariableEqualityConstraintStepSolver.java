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
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.set;
import static java.lang.Math.max;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityAndDefiningLiteralsStepSolver;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

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

	public ModelCountingOfSingleVariableEqualityConstraintStepSolver(SingleVariableEqualityConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) super.getConstraint();
	}
	
	@Override
	protected Iterable<Expression> getDefiningLiterals(Constraint2 contextualConstraint, RewritingProcess process) {
		Iterable<Expression> result = getDisequalsDisequalities(process);
		return result;
	}

	private Iterable<Expression> getDisequalsDisequalities(RewritingProcess process) {
		ArrayList<Expression> disequals = arrayListFrom(getConstraint().getDisequalsIterator());
		Iterator<PairOf<Expression>> pairs = PairOfElementsInListIterator.make(disequals);
		Iterator<Expression> disequalities =
				FunctionIterator.make(
						pairs,
						pair -> Disequality.makeWithConstantSimplification(pair.first, pair.second, process));
		Iterable<Expression> result = in(disequalities);
		// TODO: Optimization: generalize NumberOfDistinctExpressionsIsLessThanStepSolver to compute number of distinct expressions
		// to inform on a limit but without stopping when we know limit will not be reached, instead going on until we get an exact count,
		// and re-use it here to compute number 
		return result;
	}

	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(
			Constraint2 contextualConstraint, RewritingProcess process) {
		
		Expression solutionExpression;
		if (getConstraint().getEqualsIterator().hasNext()) { // variable is bound to some value
			solutionExpression = ONE;
		}
		else {
			long numberOfNonAvailableValues = computeNumberOfUniqueDisequals(contextualConstraint, process);
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

	private int computeNumberOfUniqueDisequals(Constraint2 contextualConstraint, RewritingProcess process) {
		ArrayList<Expression> disequals = arrayListFrom(getConstraint().getDisequalsIterator());
		Set<Expression> equalToAPreviousDisequal = set();
		for (int i = 0; i < disequals.size() - 1; i++) {
			for (int j = i + 1; j != disequals.size(); j++) {
				if (equalToAPreviousDisequal.contains(disequals.get(j))) {
					continue;
				}
				Expression equality = Equality.makeWithConstantSimplification(disequals.get(i), disequals.get(j), process);
				if (contextualConstraint.implies(equality, process)) {
					equalToAPreviousDisequal.add(disequals.get(j));
				}
			}
		}
		return disequals.size() - equalToAPreviousDisequal.size();
	}
}