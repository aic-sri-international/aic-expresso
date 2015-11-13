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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Iterator;
import java.util.LinkedList;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * An abstract implementation for step solvers for problems based on a propagated and defining literals.
 * <p>
 * Propagated literals are literals required to be true
 * (if they are not, the solution step returns is provided by the abstract method
 * {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied()}).
 * In fact, this class allows a generalization of propagated literals in the form of a propagated CNF.
 * <p>
 * Extensions defined which are the propagated literals and propagated CNF
 * through the abstract methods {@link #getPropagatedLiterals()} and
 * {@link #getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess)},
 * which are aggregated by method {@link #getPropagatedLiterals()}.
 * <p>
 * Defining literals are literals on which the solution of the problem depends.
 * The difference between defining literals is that the solution for when defining literals
 * are not satisfied is not necessarily fixed, and with the case with propagated literals.
 * In fact, propagated literals can be seen as a special case of defining literals
 * (ones for which the solution if case of unsatisfiability happens to be fixed),
 * and they are distinguished here for convenience purposes only.
 * Once all defining literals are checked to be defined,
 * the abstract method {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(Constraint2, RewritingProcess)}
 * is invoked to provide the problem's solution.
 * <p>
 * Such problems will typically involve a {@link Constraint2}, so this class provides
 * methods for storing that as well.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver implements ContextDependentProblemStepSolver {

	protected Constraint2 constraint;
	
	public AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver(Constraint2 constraint) {
		this.constraint = constraint;
	}
	
	public Constraint2 getConstraint() {
		return constraint;
	}
	
	/**
	 * Provides information for {@link #getPropagatedCNF(RewritingProcess)},
	 * along with {@link #getPropagatedCNFBesidesPropagatedLiterals()}.
	 * The reason for this division is convenience;
	 * when an extension only needs to define propagated literals,
	 * {@link #getPropagatedLiterals()}can be used to spare a programmer the chore of defining a CNF iterable.
	 * This method <i>must</i> be overridden if {@link #getPropagatedCNF(RewritingProcess)} is not;
	 * abstract method {@link #usingDefaultImplementationOfGetPropagatedCNF()} ensure extension programmers
	 * do not forget this.
	 * If the extension's {@link #usingDefaultImplementationOfGetPropagatedCNF()} returns true,
	 * this method must be overridden and a suitable definition provided, or an error asking for that will be thrown.
	 * Otherwise, it should not be invoked, and if it is an error complaining about that will be thrown.
	 */
	protected Iterable<Expression> getPropagatedLiterals() {
		if (usingDefaultImplementationOfGetPropagatedCNF()) {
			throw new Error("This method should have been defined in " + getClass().getSimpleName() + " but was not.");
		}
		else {
			throw new Error("This method should not have been invoked. It is not to be invoked directly, but only by the default implementation of getPropagatedCNF, which the watch usingDefaultImplementationOfGetPropagatedCNF says is not being used.");
		}
	}
	
	/**
	 * Provides information for {@link #getPropagatedCNF(RewritingProcess)},
	 * along with {@link #getPropagatedLiterals()}.
	 * The reason for this division is convenience;
	 * when an extension only needs to define propagated literals,
	 * {@link #getPropagatedLiterals()}can be used to spare a programmer the chore of defining a CNF iterable.
	 * The default implementation returns an empty iterable.
	 */
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		return new LinkedList<Iterable<Expression>>();
	}

	/**
	 * An abstract method forcing extensions to explicitly indicate whether they intend to
	 * use the default implementation of {@link #getPropagatedCNF()}
	 * (either by not overriding it, or by overriding, but invoking the super class implementation).
	 * This serves as a reminder to extending classes that they must
	 * override (define, really, as the default implementation doesn't do anything)
	 * at least {@link #getPropagatedLiterals()},
	 * since the default implementation of {@link #getPropagatedCNF(RewritingProcess)} uses
	 * that and {@link #getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess)}.
	 * 
	 * @return
	 */
	abstract protected boolean usingDefaultImplementationOfGetPropagatedCNF();
	
	/**
	 * Provides whatever needs to be known for this problem to be solved, in the form of a CNF.
	 * This default implementation simply puts together what is provided by
	 * {@link #getPropagatedLiterals()} and {@link #getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess)},
	 * which are typically a more convenient way for extensions to define this information.
	 * However, it may be useful in some cases to directly override this method;
	 * for example, if this information comes from another
	 * {@link AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver}'s own
	 * {@link #getPropagatedCNF(RewritingProcess)}.
	 * @param process
	 * @return
	 */
	public Iterable<Iterable<Expression>> getPropagatedCNF(RewritingProcess process) {
		Iterable<Iterable<Expression>> result =
				in(
						NestedIterator.make(
								fromLiteralsToCNF(getPropagatedLiterals()),
								getPropagatedCNFBesidesPropagatedLiterals(process)));
		return result;
	}

	/**
	 * Methods which extensions can override in order to provide defining literals.
	 * Default implementation returns no defining literals.
	 * @param process
	 * @return
	 */
	protected Iterable<Expression> getDefiningLiterals(RewritingProcess process) {
		return list();
	}

	/**
	 * The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 * @return The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied();

	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(Constraint2 contextualConstraint, RewritingProcess process);

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		if (getConstraint() == null) {
			return new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied());
		}
		
		SolutionStep propagatedCNFIsSatisfiedStep = cnfIsSatisfied(getPropagatedCNF(process), contextualConstraint, process);
		
		SolutionStep result;
		if (propagatedCNFIsSatisfiedStep == null) {
			result = null;
		}
		else if (propagatedCNFIsSatisfiedStep.itDepends()) {
			result = propagatedCNFIsSatisfiedStep;
		}
		else if (propagatedCNFIsSatisfiedStep.getExpression().equals(FALSE)) {
			result = new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied());
		}
		else if (propagatedCNFIsSatisfiedStep.getExpression().equals(TRUE)) {
			result = solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(contextualConstraint, process);
		}
		else {
			throw new Error("Illegal value returned");
		}
		
		return result; 
	}
	
	/**
	 * The solution to be provided if all propagated literals and splitter DNF are satisfied.
	 * Default implementation checks defining literals and, if they are all defined,
	 * delegates to abstract method
	 * {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(Constraint2, RewritingProcess)}.
	 * @param process
	 */
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Constraint2 contextualConstraint, RewritingProcess process) {
		SolutionStep definingLiteralsAreDefinedStep = 
				conjunctiveClauseIsDefined(getDefiningLiterals(process), contextualConstraint, process);
		
		SolutionStep result;
		if (definingLiteralsAreDefinedStep == null) {
			result = null;
		}
		else if (definingLiteralsAreDefinedStep.itDepends()) {
			result = definingLiteralsAreDefinedStep;
		}
		else if (definingLiteralsAreDefinedStep.getExpression().equals(TRUE)) {
			result = new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(contextualConstraint, process));
		}
		else {
			throw new Error("Illegal value returned");
		}
		
		return result; 
	}

	/**
	 * A convenience method for testing whether a CNF, represented as an iterable of iterables of Expressions,
	 * is satisfied by a contextual constraint.
	 * @param cnf
	 * @param contextualConstraint
	 * @param process
	 * @return <code>null</code> if the contextual constraint is found to be self-contradictory,
	 * an instance of {@link ItDependsOn} with a literal, if whether the CNF is satisfied or not depends on that literal,
	 * or an instance of {@link Solution} with expression {@link Expressions#TRUE} or {@link Expressions#FALSE}
	 * if whether the CNF is satisfied is already determined positively or negatively, respectively.
	 */
	public static SolutionStep cnfIsSatisfied(Iterable<Iterable<Expression>> cnf, Constraint2 contextualConstraint, RewritingProcess process) {
		for (Iterable<Expression> clause : cnf) {
			boolean clauseIsSatisfied = false;
			for (Expression literal : clause) {
				ConstraintSplitting contextualConstraintSplitting = new ConstraintSplitting(contextualConstraint, literal, process);
				
				switch (contextualConstraintSplitting.getResult()) {
				case LITERAL_IS_UNDEFINED:
					return new ItDependsOn(literal); // necessary but undefined
					// OPTIMIZATION: instead of returning this, we could look whether some clause is already unsatisfied
					// OPTIMIZATION: ItDependsOn could carry conjunctions of contextual constraint and literal,
					// and of contextual constraint and literal negation, back to client for re-use.
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
				return new Solution(FALSE);
			}
			// else move on to next clause
		}
		return new Solution(TRUE);
	}

	/**
	 * A convenience method for testing whether a conjunctive clause, represented as an iterable of Expressions,
	 * is defined by a contextual constraint (that is, all of its literals or their negations are implied by the contextual constraint).
	 * @param conjunctiveClause
	 * @param contextualConstraint
	 * @param process
	 * @return <code>null</code> if the contextual constraint is found to be self-contradictory,
	 * an instance of {@link ItDependsOn} with a literal, if whether the conjunctive clause is satisfied or not depends on that literal,
	 * or an instance of {@link Solution} with expression {@link Expressions#TRUE} or {@link Expressions#FALSE}
	 * if whether the conjunctive clause is satisfied is already determined positively or negatively, respectively.
	 */
	protected static SolutionStep conjunctiveClauseIsDefined(Iterable<Expression> conjunctiveClause, Constraint2 contextualConstraint, RewritingProcess process) {
		for (Expression literal : conjunctiveClause) {
			ConstraintSplitting contextualConstraintSplitting = new ConstraintSplitting(contextualConstraint, literal, process);

			switch (contextualConstraintSplitting.getResult()) {
			case LITERAL_IS_UNDEFINED:
				return new ItDependsOn(literal); // necessary but undefined
				// OPTIMIZATION: ItDependsOn could carry conjunctions of contextual constraint and literal,
				// and of contextual constraint and literal negation, back to client for re-use.
			case LITERAL_IS_FALSE:
			case LITERAL_IS_TRUE:
				// register and move to next literal
				contextualConstraint = contextualConstraintSplitting.getConstraintConjoinedWithDefinedValueOfLiteral();
				break;
			case CONSTRAINT_IS_CONTRADICTORY:
				return null;
			}
			// else move on to next literal
		}
		return new Solution(TRUE);
	}

	/**
	 * Convenience method that converts an <code>Iterable<Expression></code> representing a conjunctive clause to
	 * a <code>Iterator<Iterable<Expression>></code> representing an equivalent CNF.
	 * @param literals
	 * @return
	 */
	protected static Iterator<Iterable<Expression>> fromLiteralsToCNF(Iterable<Expression> literals) {
		Function<Expression, Iterable<Expression>> makeUnitClause = l -> in(iterator(l));
		//		above lambda somehow not working at Ciaran's environment, replacing with seemingly identical anonymous class object below
		//		Function<Expression, Iterable<Expression>> makeUnitClause = new Function<Expression, Iterable<Expression>>() {
		//			public Iterable<Expression> apply(Expression l) {
		//				return in(iterator(l));
		//			}
		//		};
		Iterator<Iterable<Expression>> result = FunctionIterator.make(literals, makeUnitClause);
		return result;
	}
}