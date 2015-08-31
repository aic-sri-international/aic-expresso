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

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.base.PairOf.makePairOf;

import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblem;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A step solver for the problem <code>there exists X : C</code>,
 * for <code>C</code> a {@link SingleVariableEqualityConstraint},
 * which can involve free variables and is therefore a {@link ContextDependentProblem}.
 * 
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableEqualityConstraint extends AbstractSatisfiabilityOfSingleVariableConstraint {

	public SatisfiabilityOfSingleVariableEqualityConstraint(SingleVariableEqualityConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) constraint;
	}
	
	@Override
	protected Iterable<Expression> propagatedLiterals() {
		
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEqualsToVariableIterator();
		Iterator<Expression> splitterEqualities = FunctionIterator.make(pairsOfEqualsToVariableIterator, p -> Equality.make(p.first, p.second));
		
		Iterator<PairOf<Expression>> pairsOfEqualAndDisequalToVariableIterator = pairsOfEqualAndDisequalToVariableIterator();
		Iterator<Expression> splitterDisequalities = FunctionIterator.make(pairsOfEqualAndDisequalToVariableIterator, p -> Disequality.make(p.first, p.second));
		
		Iterator<Expression> splittersIterator = new NestedIterator<>(splitterEqualities, splitterDisequalities);

		Iterable<Expression> result = in(splittersIterator);
		
		return result;
	}

	protected Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator() {
		PairOfElementsInListIterator<Expression> pairsOfPositiveAtomsIterator = new PairOfElementsInListIterator<>(getConstraint().getPositiveAtoms());
		
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator =
				FunctionIterator.make(pairsOfPositiveAtomsIterator, p -> makePairOf(p.first.get(1), p.second.get(1)));
		
		return pairsOfEqualsToVariableIterator;
	}

	protected Iterator<PairOf<Expression>> pairsOfEqualAndDisequalToVariableIterator() {
		CartesianProductIterator<String, Expression> pairsOfPositiveAndNegativeAtomsIterator =
				new CartesianProductIterator<>(map(
						"equal",    (NullaryFunction<Iterator<Expression>>) () -> getConstraint().getPositiveAtoms().iterator(),
						"disequal", (NullaryFunction<Iterator<Expression>>) () -> getConstraint().getNegativeAtoms().iterator()));
		
		Iterator<PairOf<Expression>> pairsOfEqualAndDisequalIterator =
				FunctionIterator.make(pairsOfPositiveAndNegativeAtomsIterator, m -> makePairOf(m.get("equal").get(1), m.get("disequal").get(1)));
		
		return pairsOfEqualAndDisequalIterator;
	}

	@Override
	protected SolutionStep stepGivenPropagatedLiteralsAreSatisfied(Constraint contextualConstraint, RewritingProcess process) {
		return new Solution(TRUE);
	}
}