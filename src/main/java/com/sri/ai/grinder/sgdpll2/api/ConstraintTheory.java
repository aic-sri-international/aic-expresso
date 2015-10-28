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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.grinder.library.equality.formula.FormulaUtil.isInterpretedInPropositionalLogicIncludingConditionals;
import static com.sri.ai.util.Util.addAllToSet;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An interface for constraint theories to be plugged into quantifier problems.
 * <p>
 * One of its tasks is to select and manipulate <i>splitters</i>.
 * A splitter is a literal on which DPLL splits the possible interpretations of an expression.
 * The constraintTheoryWithEquality needs to know how to simplify expressions based on the fact that a splitter is true or false,
 * as well as how to simplify a <i>solution</i> based on a splitter's being true or false into a simpler solution.
 * A solution is an if-then-else expression in which all conditions are splitters.
 * 
 * @author braz
 *
 */
@Beta
public interface ConstraintTheory extends Theory {

	boolean isSuitableFor(Expression variable, RewritingProcess process);
	
	/**
	 * Indicates whether an expression is a literal in this theory.
	 * @param expression
	 * @param process
	 * @return 
	 * @return
	 */
	default boolean isLiteral(Expression expression, RewritingProcess process) {
		if (expression.equals(TRUE) || expression.equals(FALSE)) {
			return true;
		}
		return isNonTrivialLiteral(expression, process);
	}
	
	default boolean isConjunctiveClause(Expression formula, RewritingProcess process) {
		boolean result = forAll(getConjuncts(formula), c -> isLiteral(c, process));
		return result;
	}

	/**
	 * Indicates whether an expression is a non-trivial literal in this theory.
	 * @param expression
	 * @param process
	 * @return
	 */
	boolean isNonTrivialLiteral(Expression expression, RewritingProcess process);
	
	/**
	 * Make a new single-variable constraint for this constraint theory.
	 * @param variable 
	 * @param process
	 * @return
	 */
	SingleVariableConstraint makeSingleVariableConstraint(Expression variable, RewritingProcess process);
	
	/**
	 * Indicates whether single-variable constraint solver is complete (for its variable).
	 * @return whether single-variable constraint solver is complete (for its variable).
	 */
	boolean singleVariableConstraintIsCompleteWithRespectToItsVariable();
	
	/**
	 * Indicates whether an expression is an application of a function belonging to this theory,
	 * or a constant belonging to this theory.
	 * @param term
	 * @param process
	 * @return
	 */
	boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process);

	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ContextDependentProblemStepSolver} deciding its satisfiability.
	 * @param process TODO
	 * @return a {@link ContextDependentProblemStepSolver} deciding a constraint's satisfiability.
	 */
	ContextDependentProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process);
	
	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ContextDependentProblemStepSolver} computing its model count.
	 * @param process TODO
	 * @return a {@link ContextDependentProblemStepSolver} computing a constraint's model count.
	 */
	ContextDependentProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process);
	
	/**
	 * Returns the negation of a literal.
	 * While one could simply add or remove a <code>not</code> in the original literal,
	 * this methods provides a way of generating theory-specific representations at the generic level of the {@link ConstraintTheory} interface.
	 * For example, for equality
	 * we may prefer the negation of <code>X = a</code> to be represented as <code>X != a</code> instead of <code> not X = a</code>.
	 * @param literal the literal
	 * @param process TODO
	 * @return the negation of literal
	 */
	Expression getLiteralNegation(Expression literal, RewritingProcess process);
	
	/**
	 * Provides a collection of all generalized variables (according to this theory) in a given expression,
	 * where a generalized variable is an expression that is not a boolean connective or an interpreted element in this theory
	 * (see {@link #isInterpretedInThisTheoryBesidesBooleanConnectives(Expression, RewritingProcess)}).
	 * @param expression
	 * @param process
	 * @return
	 */
	default Collection<Expression> getVariablesIn(Expression expression, RewritingProcess process) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		Predicate<Expression> isVariablePredicate = e -> isVariable(e, process);
		Iterator<Expression> variablesIterator = PredicateIterator.make(subExpressionsIterator, isVariablePredicate);
		LinkedHashSet<Expression> variables = addAllToSet(variablesIterator);
		return variables;
	}

	/**
	 * Indicates whether an expression is considered a variable in this theory,
	 * meaning that it is not a constants of any of the registered types in process
	 * (per {@link RewritingProcess#getTypes()} and {@link RewritingProcess#isUniquelyNamedConstant(Expression)}),
	 * it is not interpreted in propositional logic
	 * (per {@link FormulaUtil#isInterpretedInPropositionalLogicIncludingConditionals(Expression)}),
	 * and is not interpreted in this theory besides boolean connectives
	 * (per {@link #isInterpretedInThisTheoryBesidesBooleanConnectives(Expression, RewritingProcess)}.
	 * @param expression
	 * @param process
	 * @return
	 */
	default boolean isVariable(Expression expression, RewritingProcess process) {
		boolean result =
				!process.isUniquelyNamedConstant(expression)
				&& !isInterpretedInPropositionalLogicIncludingConditionals(expression)  
				&& !isInterpretedInThisTheoryBesidesBooleanConnectives(expression, process)
				&& !thereExists(process.getTypes(), t -> t.contains(expression));
		return result;
	}

	//////////// AUTOMATIC TESTING 
	
	/** Sets variables to be used in randomly generated literals. */
	void setVariableNamesAndTypeNamesForTesting(Map<String, String> variableNamesForTesting);
	
	/** Gets variables to be used in randomly generated literals. */
	Map<String, String> getVariableNamesAndTypeNamesForTesting();

	/** Returns a set of types appropriate for testing this constraint theory. */
	Collection<Type> getTypesForTesting();
	
	/** Sets a set of types appropriate for testing this constraint theory. */
	void setTypesForTesting(Collection<Type> newTypesForTesting);
	
	/**
	 * Returns the variable on which testing literals are generated.
	 * @return the variable on which testing literals are generated
	 */
	String getTestingVariable();
	
	void setTestingVariable(String newTestingVariable);
	
	RewritingProcess extendWithTestingInformation(RewritingProcess process);
	
	/**
	 * Returns a random atom in this constraint theory on a given variable.
	 * This is useful for making random constraints for correctness and performance testing.
	 * @param random a random generator
	 * @param process a rewriting process
	 */
	Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process);
	
	/**
	 * Same as {@link #makeRandomAtomOn(String, Random, RewritingProcess),
	 * but applied randomly to one of the test variables.
	 */
	default Expression makeRandomAtom(Random random, RewritingProcess process) {
		String variableToBeUsed = Util.pickUniformly(getVariableNamesAndTypeNamesForTesting().keySet().iterator(), random);
		Expression result = makeRandomAtomOn(variableToBeUsed, random, process);
		return result;
	}
	
	/**
	 * Same as {@link #makeRandomAtomOn(String, Random, RewritingProcess) for testing variable
	 * (returned by {@link #getTestingVariable()}).
	 */
	default Expression makeRandomAtomOnTestingVariable(Random random, RewritingProcess process) {
		Expression result = makeRandomAtomOn(getTestingVariable(), random, process);
		return result;
	}

	/**
	 * Returns a random literal in this constraint theory on the testing variable.
	 * This is useful for making random constraints for correctness and performance testing.
	 * @param random a random generator
	 * @param process a rewriting process
	 */
	default Expression makeRandomLiteralOnTestingVariable(Random random, RewritingProcess process) {
		String variable = getTestingVariable();
		return makeRandomLiteralOn(variable, random, process);
	}

	/**
	 * @param variable
	 * @param random
	 * @param process
	 * @return
	 */
	default Expression makeRandomLiteralOn(String variable, Random random, RewritingProcess process) {
		Expression atom = makeRandomAtomOn(variable, random, process);
		Expression literal = random.nextBoolean()? atom : Not.make(atom);
		return literal;
	}

	/**
	 * Same as {@link #makeRandomLiteralOn(String, Random, RewritingProcess),
	 * but applied randomly to one of the testing variables.
	 */
	default Expression makeRandomLiteral(Random random, RewritingProcess process) {
		String variableToBeUsed = Util.pickUniformly(getVariableNamesAndTypeNamesForTesting().keySet().iterator(), random);
		Expression result = makeRandomLiteralOn(variableToBeUsed, random, process);
		return result;
	}
	
}