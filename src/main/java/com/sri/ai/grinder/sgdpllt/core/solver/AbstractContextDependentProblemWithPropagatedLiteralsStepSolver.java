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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.storeIterableOfIterablesInArrayListOfArrayLists;

import java.util.ArrayList;
import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * An abstract implementation for step solvers for problems based on a propagated literals, and a propagated CNF.
 * <p>
 * Propagated literals are literals required to be true
 * (if they are not, the solver step returns is provided by the abstract method
 * {@link #getSolutionExpressionGivenContradiction()}).
 * In fact, this class allows a generalization of propagated literals in the form of a propagated CNF.
 * <p>
 * Extensions defined which are the propagated literals and propagated CNF
 * through the abstract methods {@link #getPropagatedLiterals(Context)} and
 * {@link #getPropagatedCNFBesidesPropagatedLiterals(Context)},
 * which are aggregated by method {@link #getPropagatedLiterals(Context)}.
 * <p>
 * Such problems will typically involve a {@link Constraint}, so this class provides
 * methods for storing that as well.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractContextDependentProblemWithPropagatedLiteralsStepSolver implements ContextDependentExpressionProblemStepSolver {

	final static boolean MAKE_SUB_STEP_SOLVERS_THAT_START_TO_CHECK_PROPAGATED_CNF_FROM_WHERE_THIS_ONE_LEFT_OFF = false;
	
	protected Constraint constraint;
	
	private ArrayList<ArrayList<Expression>> cachedPropagatedCNF;
	private int initialClauseToConsiderInPropagatedCNF = 0;
	private int initialLiteralToConsiderInInitialClauseToConsiderInPropagatedCNF = 0;

	public AbstractContextDependentProblemWithPropagatedLiteralsStepSolver(Constraint constraint) {
		this.constraint = constraint;
	}

	/**
	 * Make a clone of this step solver but set it to ignore clauses and literals of the propagated CNF
	 * that have already being checked so far.
	 * @param clauseIndex TODO
	 * @param literalIndex TODO
	 * @return
	 */
	private AbstractContextDependentProblemWithPropagatedLiteralsStepSolver makeCopyConsideringPropagatedCNFFromNowOn(int clauseIndex, int literalIndex) {
		AbstractContextDependentProblemWithPropagatedLiteralsStepSolver result = clone();
		result.initialClauseToConsiderInPropagatedCNF = clauseIndex;
		result.initialLiteralToConsiderInInitialClauseToConsiderInPropagatedCNF = literalIndex;
		return result;
	}
	
	/**
	 * A cloning method delegating to super.clone().
	 */
	@Override
	public AbstractContextDependentProblemWithPropagatedLiteralsStepSolver clone() {
		try {
			return (AbstractContextDependentProblemWithPropagatedLiteralsStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error("Trying to clone " + getClass() + " but cloning is not supported for this class.");
		}
	}
	
	public Constraint getConstraint() {
		return constraint;
	}
	
	/**
	 * Provides information for default implementation of {@link #makePropagatedCNF(Context)},
	 * along with {@link #getPropagatedCNFBesidesPropagatedLiterals()}.
	 * The reason for this division is convenience;
	 * when an extension only needs to define propagated literals,
	 * {@link #getPropagatedLiterals(Context)}can be used to spare a programmer the chore of defining a CNF iterable.
	 * This method <i>must</i> be overridden if {@link #makePropagatedCNF(Context)} is not;
	 * abstract method {@link #usingDefaultImplementationOfMakePropagatedCNF()} ensure extension programmers
	 * do not forget this.
	 * If the extension's {@link #usingDefaultImplementationOfMakePropagatedCNF()} returns true,
	 * this method must be overridden and a suitable definition provided, or an error asking for that will be thrown.
	 * Otherwise, it should not be invoked, and if it is an error complaining about that will be thrown.
	 * @param context
	 */
	protected Iterable<Expression> getPropagatedLiterals(Context context) {
		if (usingDefaultImplementationOfMakePropagatedCNF()) {
			throw new Error("This method should have been defined in " + getClass().getSimpleName() + " but was not.");
		}
		else {
			throw new Error("This method should not have been invoked. It is not to be invoked directly, but only by the default implementation of getPropagatedCNF, which the watch usingDefaultImplementationOfGetPropagatedCNF says is not being used.");
		}
	}
	
	/**
	 * Provides information for default implementation of {@link #makePropagatedCNF(Context)},
	 * along with {@link #getPropagatedLiterals(Context)}.
	 * The reason for this division is convenience;
	 * when an extension only needs to define propagated literals,
	 * {@link #getPropagatedLiterals(Context)}can be used to spare a programmer the chore of defining a CNF iterable.
	 */
	abstract protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(Context context);

	/**
	 * An abstract method forcing extensions to explicitly indicate whether they intend to
	 * use the default implementation of {@link #makePropagatedCNF(Context context)}
	 * (either by not overriding it, or by overriding, but invoking the super class implementation).
	 * This serves as a reminder to extending classes that they must
	 * override (define, really, as the default implementation doesn't do anything other than error checking)
	 * at least {@link #getPropagatedLiterals(Context)},
	 * since the default implementation of {@link #makePropagatedCNF(Context)} uses
	 * that and {@link #getPropagatedCNFBesidesPropagatedLiterals(Context)}.
	 * 
	 * @return
	 */
	abstract protected boolean usingDefaultImplementationOfMakePropagatedCNF();
	
	/**
	 * Checks if there is a cached propagated CNF stored and, if not,
	 * computes it with {@link #makePropagatedCNF(Context)}.
	 * @param context
	 * @return
	 */
	protected ArrayList<ArrayList<Expression>> getPropagatedCNF(Context context) {
		
		if (cachedPropagatedCNF == null) {
			cachedPropagatedCNF = makePropagatedCNF(context);
		}
		
//		System.out.println("\nconstraint: " + constraint);	
//		System.out.println("propagated literals: " + join(cachedPropagatedCNF.iterator()));

		return cachedPropagatedCNF;
	}

	/**
	 * Makes a CNF that, if not satisfied, means the solution
	 * is {@link #getSolutionExpressionGivenContradiction()}.
	 * If it is satisfied,
	 * then the step solver will invoke {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context)}.
	 * <p>
	 * Note that the result of this method is cached and provided by {@link #getPropagatedCNF(Context)}.
	 * Extensions should override <i>this<i> method in order to keep the caching behavior.
	 * <p>
	 * This default implementation simply puts together what is provided by
	 * {@link #getPropagatedLiterals(Context)} and {@link #getPropagatedCNFBesidesPropagatedLiterals(Context)},
	 * which are typically a more convenient way for extensions to define this information.
	 * However, it may be useful in some cases to directly override this method;
	 * for example, if this information comes from another
	 * {@link AbstractContextDependentProblemWithPropagatedLiteralsStepSolver}'s own
	 * {@link #makePropagatedCNF(Context)}.
	 * @param context
	 * @return
	 */
	protected ArrayList<ArrayList<Expression>> makePropagatedCNF(Context context) {
		ArrayList<ArrayList<Expression>> result;
		
		Iterable<Iterable<Expression>> propagatedCNFIterable =
				in(
						NestedIterator.make(
								fromLiteralsToCNF(getPropagatedLiterals(context)),
								getPropagatedCNFBesidesPropagatedLiterals(context)));
		
		result = storeIterableOfIterablesInArrayListOfArrayLists(propagatedCNFIterable);
		
		return result;
	}

	/**
	 * The solution to be provided if any of the propagated literals is not satisfied by the context.
	 * @return The solution to be provided if any of the propagated literals is not satisfied by the context.
	 */
	protected abstract Expression getSolutionExpressionGivenContradiction();

	protected abstract SolverStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context context);

	@Override
	public SolverStep step(Context context) {
		if (getConstraint().isContradiction()) {
			return new Solution(getSolutionExpressionGivenContradiction());
		}
		
		SolverStep propagatedCNFIsSatisfiedStep = cnfIsSatisfied(getPropagatedCNF(context), context);
		
		SolverStep result;
		if (propagatedCNFIsSatisfiedStep == null) {
			result = null;
		}
		else if (propagatedCNFIsSatisfiedStep.itDepends()) {
			result = propagatedCNFIsSatisfiedStep;
		}
		else if (propagatedCNFIsSatisfiedStep.getValue().equals(FALSE)) {
			result = new Solution(getSolutionExpressionGivenContradiction());
		}
		else if (propagatedCNFIsSatisfiedStep.getValue().equals(TRUE)) {
			result = solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(context);
		}
		else {
			throw new Error("Illegal value returned");
		}
		
		return result; 
	}
	
	/**
	 * A convenience method for testing whether a CNF, represented as an iterable of iterables of Expressions,
	 * is satisfied by a context.
	 * @param cnf
	 * @param context
	 * @return <code>null</code> if the context is found to be self-contradictory,
	 * an instance of {@link ItDependsOn} with a literal, if whether the CNF is satisfied or not depends on that literal,
	 * or an instance of {@link Solution} with expression {@link Expressions#TRUE} or {@link Expressions#FALSE}
	 * if whether the CNF is satisfied is already determined positively or negatively, respectively.
	 */
	protected SolverStep cnfIsSatisfied(ArrayList<ArrayList<Expression>> cnf, Context context) {
		// note the very unusual initialization of literalIndex
		// this is due to our wanting to be initialized to initialLiteralToConsiderInInitialClauseToConsiderInPropagatedCNF,
		// but only the first time the loop is executed (that is, inside the first clause loop)
		// We therefore start the method by initializing it to initialLiteralToConsiderInInitialClauseToConsiderInPropagatedCNF,
		// and then initialize it to 0 when clauseIndex is iterated.
		int literalIndex = initialLiteralToConsiderInInitialClauseToConsiderInPropagatedCNF;
		for (int clauseIndex = initialClauseToConsiderInPropagatedCNF ;
				clauseIndex != cnf.size();
				clauseIndex++, literalIndex = 0) { // unusual! See above
			
			ArrayList<Expression> clause = cnf.get(clauseIndex);
			boolean clauseIsSatisfied = false;
			for ( /* literalIndex already initialized at this point */ ; literalIndex != clause.size(); literalIndex++) {
				Expression literal = clause.get(literalIndex);
				ContextSplitting split = new ContextSplitting(literal, context);
				
				switch (split.getResult()) {
				case LITERAL_IS_UNDEFINED:
					AbstractContextDependentProblemWithPropagatedLiteralsStepSolver subStepSolver
					= MAKE_SUB_STEP_SOLVERS_THAT_START_TO_CHECK_PROPAGATED_CNF_FROM_WHERE_THIS_ONE_LEFT_OFF
					? makeCopyConsideringPropagatedCNFFromNowOn(clauseIndex, literalIndex)
							: this;
					return new ItDependsOn(literal, split, subStepSolver, subStepSolver); // literal is necessary, but undefined
					// OPTIMIZATION: instead of returning the first undefined literal, we could look whether some clause is already unsatisfied
				case LITERAL_IS_TRUE:
					clauseIsSatisfied = true;
					context = split.getContextAndLiteral();
					break;
				case LITERAL_IS_FALSE:
					context = split.getContextAndLiteralNegation();
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
	 * is defined by a context (that is, all of its literals or their negations are implied by the context).
	 * @param conjunctiveClause
	 * @param context
	 * @return <code>null</code> if the context is found to be self-contradictory,
	 * an instance of {@link ItDependsOn} with a literal, if whether the conjunctive clause is satisfied or not depends on that literal,
	 * or an instance of {@link Solution} with expression {@link Expressions#TRUE} or {@link Expressions#FALSE}
	 * if whether the conjunctive clause is satisfied is already determined positively or negatively, respectively.
	 */
	protected SolverStep conjunctiveClauseIsDefined(Iterable<Expression> conjunctiveClause, Context context) {
		for (Expression literal : conjunctiveClause) {
			ContextSplitting split = new ContextSplitting(literal, context);

			switch (split.getResult()) {
			case LITERAL_IS_UNDEFINED:
				return new ItDependsOn(literal, split, clone(), clone()); // necessary but undefined
			case LITERAL_IS_TRUE:
				// register in context that literal is true and move to next literal
				context = split.getContextAndLiteral();
				break;
			case LITERAL_IS_FALSE:
				// register in context that literal is false and move to next literal
				context = split.getContextAndLiteralNegation();
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