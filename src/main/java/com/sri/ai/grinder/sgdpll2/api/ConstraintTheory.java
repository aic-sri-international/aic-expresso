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
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.grinder.library.equality.formula.FormulaUtil.isInterpretedInPropositionalLogicIncludingConditionals;
import static com.sri.ai.util.Util.addAllToSet;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.thereExists;

import java.util.ArrayList;
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
import com.sri.ai.grinder.core.DefaultRewritingProcess;
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

	boolean isSuitableFor(Expression variable, Type type);
	
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
	 * Indicates whether an expression is a non-trivial atom in this theory.
	 * @param expression
	 * @param process
	 * @return
	 */
	boolean isNonTrivialAtom(Expression expression, RewritingProcess process);
	
	/**
	 * Indicates whether an expression is a non-trivial literal in this theory.
	 * This is defined as its being either a non-trivial atom, or a non-trivial negative literals,
	 * which in turn is the negation of a non-trivial atom.
	 * @param expression
	 * @param process
	 * @return
	 */
	default boolean isNonTrivialLiteral(Expression expression, RewritingProcess process) {
		boolean result = isNonTrivialAtom(expression, process) || isNonTrivialNegativeLiteral(expression, process);
		return result;
	}

	default boolean isNonTrivialNegativeLiteral(Expression expression, RewritingProcess process) {
		boolean result = expression.hasFunctor(NOT) && isNonTrivialAtom(expression.get(0), process);
		return result;
	}
	
	/**
	 * Make a new single-variable constraint for this constraint theory.
	 * @param variable 
	 * @param constraintTheory the constraint theory of the application (not necessarily <code>this</code> because <code>this</code> may be a sub-constraint theory in a compound one
	 * @param process
	 * @return
	 */
	SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process);
	
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
	 * a {@link ContextDependentExpressionProblemStepSolver} deciding its satisfiability.
	 * @param process TODO
	 * @return a {@link ContextDependentExpressionProblemStepSolver} deciding a constraint's satisfiability.
	 */
	ContextDependentExpressionProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process);
	
	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ContextDependentExpressionProblemStepSolver} computing its model count.
	 * @param process TODO
	 * @return a {@link ContextDependentExpressionProblemStepSolver} computing a constraint's model count.
	 */
	ContextDependentExpressionProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process);
	
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
	void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesForTesting);
	
	/** Gets variables to be used in randomly generated literals. */
	Map<String, Type> getVariableNamesAndTypesForTesting();

	/**
	 * Returns the variable names as returned by {@link #getVariableNamesAndTypesForTesting()}
	 * as an array list (this is cached and updated as needed).
	 * @return
	 */
	ArrayList<String> getVariableNamesForTesting();

	/** Returns a set of types appropriate for testing this constraint theory. */
	Collection<Type> getTypesForTesting();
	
	/** Sets a set of types appropriate for testing this constraint theory. */
	void setTypesForTesting(Collection<Type> newTypesForTesting);
	
//	/** Samples a uniquely named constant of given appropriate for testing this constraint theory. */
//	Expression sampleUniquelyNamedConstantsForTesting(Type type);
//	
//	/**
//	 * Sets an iterable of uniquely named constants for given type appropriate for testing this constraint theory.
//	 * Note that all types must have testing uniquely named constants associated with them.
//	 * If you want a type to simply use {@link Type#sampleConstant(Random)},
//	 * use methods {@link #setUseTypeUniquelyNamedConstantSampling(Type, boolean)}
//	 * or {@link #setUseTypeUniquelyNamedConstantSamplingForAllTypes(boolean)}.
//	 */
//	void setUniquelyNamedConstantsForTesting(Type type, Iterable<Expression> newUniquelyNamedConstantsForTesting);
//	
//	/**
//	 * Indicates whether the sampling of a type's uniquely named constant for testing must
//	 * use the types {@link Type#sampleConstant(Random)}.
//	 * @return
//	 */
//	boolean getUseTypeUniquelyNamedConstantSampling(Type type);
//	
//	/**
//	 * Sets the flag indicating whether the sampling of a type's uniquely named constant for testing must
//	 * use the types {@link Type#sampleConstant(Random)}.
//	 * @return
//	 */
//	void setUseTypeUniquelyNamedConstantSampling(Type type, boolean newValue);
//	
//	/**
//	 * Indicates whether the sampling of <i>all</i> types uniquely named constant for testing must
//	 * use the types {@link Type#sampleConstant(Random)}.
//	 * @return
//	 */
//	boolean getUseTypeUniquelyNamedConstantSamplingForAllTypes();
//	
//	/**
//	 * Sets the flag indicating whether the sampling of <i>all</i> types uniquely named constant for testing must
//	 * use the types {@link Type#sampleConstant(Random)}.
//	 * @return
//	 */
//	void setUseTypeUniquelyNamedConstantSamplingForAllTypes(boolean newValue);
//	
	/**
	 * Picks one of the testing variables returned by {@link #getTestingVariables()}
	 * with uniform probability.
	 * @param random
	 * @return
	 */
	default String pickTestingVariableAtRandom(Random random) {
		String result = Util.pickUniformly(getVariableNamesAndTypesForTesting().keySet(), random);
		return result;
	}
	
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
		String variableToBeUsed = Util.pickUniformly(getVariableNamesAndTypesForTesting().keySet().iterator(), random);
		Expression result = makeRandomAtomOn(variableToBeUsed, random, process);
		return result;
	}
	
	/**
	 * Same as {@link #makeRandomAtomOn(String, Random, RewritingProcess) for testing variable
	 * (returned by {@link #pickTestingVariableAtRandom()}).
	 */
	default Expression makeRandomAtomOnTestingVariable(Random random, RewritingProcess process) {
		Expression result = makeRandomAtomOn(pickTestingVariableAtRandom(random), random, process);
		return result;
	}

	/**
	 * @param variable
	 * @param random
	 * @param process
	 * @return
	 */
	default Expression makeRandomLiteralOn(String variable, Random random, RewritingProcess process) {
		Expression atom = makeRandomAtomOn(variable, random, process);
		Expression literal = random.nextBoolean()? atom : getLiteralNegation(atom, process);
		return literal;
	}

	/**
	 * Same as {@link #makeRandomLiteralOn(String, Random, RewritingProcess),
	 * but applied randomly to one of the testing variables.
	 */
	default Expression makeRandomLiteral(Random random, RewritingProcess process) {
		String variableToBeUsed = pickTestingVariableAtRandom(random);
		Expression result = makeRandomLiteralOn(variableToBeUsed, random, process);
		return result;
	}

	RewritingProcess extendWithTestingInformation(RewritingProcess process);

	default RewritingProcess makeRewritingProcessWithTestingInformation() {
		return extendWithTestingInformation(new DefaultRewritingProcess(null));
	}
}