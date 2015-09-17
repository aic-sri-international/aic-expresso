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
package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;

import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * An abstract implementation for step solvers for boolean-valued problems based on propagated literals and CNF.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractContextDependentProblemWithPropagatedLiteralsStepSolver implements ContextDependentProblemStepSolver {

	/**
	 * An iterable over the propagated literals from this problem, that is, the necessary
	 * conditions necessary so that the solution for this problem is TRUE.
	 * A propagated literal can be considered a splitter which, if false, renders the whole problem false.
	 * Because they are required, propagated literals are checked first.
	 * @return the propagated literals from this problem.
	 */
	protected abstract Iterable<Expression> propagatedLiterals();
	
	/**
	 * An iterable over a propagated CNF from this problem, that is, the necessary
	 * clauses necessary so that the solution for this problem is TRUE.
	 * Note that we did not have to define {@link #propagatedLiterals()},
	 * and could have instead required propagated literals to be provided as unit clauses in this CNF.
	 * Its definition is for convenience only, since many extensions will only
	 * need to define propagated literals and return an empty propagated CNF.
	 * @return the propagated CNF from this problem.
	 */
	abstract protected Iterable<Iterable<Expression>> getPropagatedCNF(RewritingProcess process);

	/**
	 * The solution to be provided if all propagated literals and splitter DNF are satisfied..
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied();

	/**
	 * The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 * @return The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied();

	@Override
	public SolutionStep step(Constraint contextualConstraint, RewritingProcess process) {
	
		Iterator<Iterable<Expression>> propagatedLiteralsCNF = FunctionIterator.make(propagatedLiterals(), l -> in(iterator(l)));
		
		Iterable<Iterable<Expression>> propagatedCNF =
				in(NestedIterator.<Iterable<Expression>>make(propagatedLiteralsCNF, getPropagatedCNF(process).iterator()));
		
		for (Iterable<Expression> clause : propagatedCNF) {
			boolean clauseIsSatisfied = false;
			for (Expression literal : clause) {
				ConstraintSplitting contextualConstraintSplitting = new ConstraintSplitting(contextualConstraint, literal, process);
				
				switch (contextualConstraintSplitting.getResult()) {
				case LITERAL_IS_UNDEFINED:
					return new ItDependsOn(literal); // necessary but undefined
					// OPTIMIZATION: instead of returning this, we could look whether some clause is already unsatisfied
					// OPTIMIZATION: ItDependsOn could carry conjunctions of contextual constraint and literal, and of contextual constraint and literal negation,
					// back to client for re-use.
				case LITERAL_IS_TRUE:
					clauseIsSatisfied = true; // note that there is no 'break' in this case, so we move on to update the contextual constraint below
				case LITERAL_IS_FALSE:
					contextualConstraint = contextualConstraintSplitting.getConstraintConjoinedWithDefinedValueOfLiteral();
					break;
				case CONSTRAINT_IS_CONTRADICTORY:
					return null;
				}
				
				if (clauseIsSatisfied) {
					break; // no need to examine remaining literals in clause
				}
			}
			
			if ( ! clauseIsSatisfied) {
				// clause is false, so the whole CNF is false
				return new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied());
			}
			// else move on to next clause
		}

		SolutionStep result = new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied());
		return result; 
	}
}