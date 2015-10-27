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
import static com.sri.ai.grinder.library.boole.And.isConjunction;
import static com.sri.ai.util.Util.myAssert;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;

@Beta
public interface Constraint2 extends Constraint {

	ConstraintTheory getConstraintTheory();
	
	/**
	 * Returns an {@link ConstraintTheoryTester} representing the conjunction of this constraint and
	 * a given literal, or null if they are contradictory.
	 * <p>
	 * At this point, the formula should be either a literal, or a {@link Constraint2}.
	 * 
	 * @param literal the literal to be conjoined.
	 * @param process the rewriting process
	 * @return the application result or <code>null</code> if contradiction.
	 */
	Constraint2 conjoinWithLiteral(Expression literal, RewritingProcess process);
	
	/**
	 * Tests whether a literal is contradictory with this constraint
	 * by checking whether conjoining it with the literal's negation produces a contradiction.
	 * @param literal
	 * @param process
	 * @return
	 */
	default boolean implies(Expression literal, RewritingProcess process) {
		Expression literalNegation = getConstraintTheory().getLiteralNegation(literal, process);
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
		Constraint2 conjunction = conjoin(formula, process);
		boolean result = conjunction == null;
		return result;
	}

	/**
	 * Returns an {@link ConstraintTheoryTester} representing the conjunction of this constraint and
	 * a given formula, or null if they are contradictory.
	 * <p>
	 * At this point, the formula should be either a literal, or a {@link Constraint2}.
	 * <p>
	 * Extensions may want to override this method if there are more efficient ways
	 * of conjoining with (certain types of) constraints than simply treating them as a formula
	 * 
	 * @param formula the formula to be conjoined.
	 * @param process the rewriting process
	 * @return the application result or <code>null</code> if contradiction.
	 */
	default Constraint2 conjoin(Expression formula, RewritingProcess process) {
		myAssert(
				() -> isValidConjoinant(formula, process),
				() -> this.getClass() + " currently only supports conjoining with literals, conjunctive clauses, and constraints, but received " + formula);
		
		Constraint2 result;
	
		if (formula instanceof Constraint2 || isConjunction(formula)) {
			result = conjoinWithConjunctiveClause(formula, process); // for now, all Constraints are conjunctions. This will probably change in the future.
		}
		else {
			result = conjoinWithLiteral(formula, process);
		}
	
		return result;
	}

	/**
	 * @param formula
	 * @param process
	 * @return
	 */
	default boolean isValidConjoinant(Expression formula, RewritingProcess process) {
		boolean result =
				formula instanceof Constraint2
				|| getConstraintTheory().isConjunctiveClause(formula, process);
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
	default Constraint2 conjoinWithConjunctiveClause(Expression conjunctiveClause, RewritingProcess process) {
		Constraint2 result;
		List<Expression> conjuncts = getConjuncts(conjunctiveClause);
		if (conjuncts.size() == 1) { // this is necessary to avoid an infinite loop
			result = conjoinWithLiteral(conjuncts.get(0), process);
		}
		else {
			result = this;
			for (Expression literal : conjuncts) {
				result = result.conjoin(literal, process);
				if (result == null) {
					break;
				}
			}
		}
		return result;
	}
	
//	/**
//	 * Returns, in time constant in the size of the constraint,
//	 * a pair of {@link Constraint}s whose conjunction is equivalent to this constraint,
//	 * such that the given variables only occur in the second one.
//	 * <p>
//	 * Note that there may be multiple such decompositions,
//	 * and that <code>Pair(new {@link ExpressionConstraint}(TRUE), this)</code> is such a decomposition.
//	 * Implementations must seek to minimize the size of the second constraint while keeping
//	 * time constant in the size of the original constraint.
//	 * <p>
//	 * The point of this operation is to isolate the variables in the second constraint,
//	 * while preserving as much internal efficient representation about the remaining variables
//	 * in the first constraint.
//	 * <p>
//	 * For example, suppose we have a complex, efficiently represented constraint
//	 * <code>C</code> on variables <code>X,Y</code>, which gets conjoined with an
//	 * also efficiently represented constraint <code>C'</code> in <code>Z</code>
//	 * (which could involve <code>X</code> or <code>Y</code> or both),
//	 * producing a new constraint <code>C''</code>.
//	 * Ideally, the internal representation of <code>C''</code> preserves
//	 * the original efficient representations.
//	 * If now we want to compute, say, <code>there exists Z : C''</code>
//	 * @return
//	 */
//	PairOf<Constraint> decomposeInConstantTime(Collection<Expression> variables);
	
//	
//	/**
//	 * Given a sub-set of supported indices, projects the constraint onto the remaining ones.
//	 * Resulting constraint still supports all original indices.
//	 * Default implementation uses symbolic satisfiability through {@link SGDPLLT}.
//	 * Specific constraint implementations will typically have more efficient ways to do it.
//	 */
//	default Constraint project(Collection<Expression> eliminatedIndices, RewritingProcess process) {
//		Expression resultExpression =
//				SymbolicSolver.solve(
//						new BooleansWithConjunctionGroup(),
//						eliminatedIndices,
//						condition,
//						body,
//						getConstraintTheory().makeSingleVariableConstraint(null),
//						process);
//		// note that solvers should be aware that their input or part of their input may be a Constraint, and take advantage of the internal representations already present in them, instead of simply converting them to an Expression and redoing all the work.
//		Collection<Expression> remainingSupportedIndices = Util.subtract(getSupportedIndices(), eliminatedIndices);
//		Constraint result = ExpressionConstraint.wrap(getConstraintTheory(), remainingSupportedIndices, resultExpression);
//		return result;
//	}
}