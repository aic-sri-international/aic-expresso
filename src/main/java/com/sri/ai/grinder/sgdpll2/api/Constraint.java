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
package com.sri.ai.grinder.sgdpll2.api;

import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.util.Util.myAssert;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;

@Beta
public interface Constraint extends Expression {

	ConstraintTheory getConstraintTheory();
	
	/**
	 * Returns an {@link ConstraintTheoryTester} representing the conjunction of this constraint and
	 * a given literal, or null if they are contradictory.
	 * <p>
	 * At this point, the formula should be either a literal, or a {@link Constraint}.
	 * 
	 * @param literal the literal to be conjoined.
	 * @param process the rewriting process
	 * @return the application result or <code>null</code> if contradiction.
	 */
	Constraint conjoinWithLiteral(Expression literal, RewritingProcess process);
	
	/**
	 * Tests whether a literal is contradictory with this constraint
	 * by checking whether conjoining it with the literal's negation produces a contradiction.
	 * @param literal
	 * @param process
	 * @return
	 */
	default boolean implies(Expression literal, RewritingProcess process) {
		Expression literalNegation = getConstraintTheory().getLiteralNegation(literal);
		boolean result = contradictoryWith(literalNegation, process);
		return result;
	}

	/**
	 * Tests whether a formula is contradictory with this constraint
	 * by checking whether conjoining them produces a contradiction.
	 * @param formula
	 * @param process
	 * @return
	 */
	default boolean contradictoryWith(Expression formula, RewritingProcess process) {
		Constraint conjunction = conjoin(formula, process);
		boolean result = conjunction == null;
		return result;
	}

	/**
	 * Returns an {@link ConstraintTheoryTester} representing the conjunction of this constraint and
	 * a given formula, or null if they are contradictory.
	 * <p>
	 * At this point, the formula should be either a literal, or a {@link Constraint}.
	 * <p>
	 * Extensions may want to override this method if there are more efficient ways
	 * of conjoining with (certain types of) constraints than simply treating them as a formula
	 * 
	 * @param formula the formula to be conjoined.
	 * @param process the rewriting process
	 * @return the application result or <code>null</code> if contradiction.
	 */
	default Constraint conjoin(Expression formula, RewritingProcess process) {
		myAssert(
				() -> getConstraintTheory().isLiteral(formula, process) || formula instanceof Constraint,
				() -> this.getClass() + " currently only supports conjoining with literals and constraints, but received " + formula);
		
		Constraint result;
	
		if (formula instanceof Constraint) {
			result = conjoinWithConjunctiveClause(formula, process); // for now, all Constraints are conjunctions. This will probably change in the future.
		}
		else {
			result = conjoinWithLiteral(formula, process);
		}
	
		return result;
	}

	/**
	 * Returns the result of conjoining this constraint with all literal conjuncts of a given conjunctive clause
	 * (note that if <code>conjunction</code> is not an application of <code>and</code>,
	 * it will be considered a unit conjunction with itself the only conjunct.
	 * @param conjunctiveClause
	 * @param process
	 * @return the result of conjoining this constraint with all conjuncts of a given conjunction
	 */
	default Constraint conjoinWithConjunctiveClause(Expression conjunctiveClause, RewritingProcess process) {
		Constraint result = this;
		for (Expression literal : getConjuncts(conjunctiveClause)) {
			result = result.conjoin(literal, process);
			if (result == null) {
				break;
			}
		}
		return result;
	}
}