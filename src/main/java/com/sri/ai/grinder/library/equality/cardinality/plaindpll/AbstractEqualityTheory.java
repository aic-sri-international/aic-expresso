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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * An abstract class for a {@link Theory} including equality literals
 * on generalized (relational) variables,
 * but possibly dealing with more types of constraints (such as <, for example).
 * <p>
 * It defines generic code for such theories while leaving abstract methods
 * that specify specific behavior.
 * Its extension {@link EqualityTheory} defines these methods for a theory with
 * = and != constraints only.
 * Someone wishing to develop a theory including equality but also other constraints
 * should extend {@link EqualityTheory} and override most of the methods
 * defines as abstract here, even if they are defined there (because they do not consider other types
 * of constraints).
 */
public abstract class AbstractEqualityTheory extends AbstractTheory {
	
	public TermTheory termTheory;
	
	// Important:
	// this class generalizes the notion of a variable to a "generalized variable" (simply referred by as "variable"),
	// which is either a variable symbol, or an uninterpreted function application such as p(a, b, X).
	// It can also be seen as an indexed variable (typically represented as x_i, y_i,j etc).
	
	public AbstractEqualityTheory(TermTheory termTheory) {
		super();
		this.termTheory = termTheory;
	}

	///////////// ABSTRACT METHODS
	
	@Override
	abstract public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers();

	@Override
	abstract public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers();

	/**
	 * If expression can generate a splitter, returns the appropriate splitter's functor;
	 * for example, an expression a != b generates the splitter =,
	 * so the result for that input will be =.
	 * If expression cannot generate a splitter, returns <code>null</code>.
	 * @param expression
	 * @return
	 */
	abstract protected String getCorrespondingSplitterFunctorOrNull(Expression expression);
	
	@Override
	abstract public Constraint makeConstraint(Collection<Expression> indices);
	
	///////////// END OF ABSTRACT METHODS

	// FROM NOW ON CODE MUST BE GENERIC
	
	@Override
	protected boolean isVariableTerm(Expression term, RewritingProcess process) {
		return termTheory.isVariableTerm(term, process);
	}

	/**
	 * If expression can originate a splitter and has at least one variable argument, returns the splitter by making it in the following way:
	 * obtain the appropriate splitter functor from {@link #getCorrespondingSplitterFunctorOrNull(Expression)}
	 * and create splitter by getting expression's first variable argument, V, and expression's first argument distinct from V.
	 * Otherwise, returns <code>null</code>.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Expression result = null;
		String splitterFunctor = getCorrespondingSplitterFunctorOrNull(expression);
		if (splitterFunctor != null) {
			// remember that equality can have an arbitrary number of terms
			Expression variable  = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), 
					e -> isVariableTerm(e, process));
			if (variable != null) {
				Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(
						expression.getArguments(),
						e -> ! e.equals(variable) && termTheory.isTerm(e, process));
				if (otherTerm != null) {
					result = makeSplitterFromFunctorAndTwoTerms(splitterFunctor, variable, otherTerm, indices, process);
				}
			}
		}
		return result;
	}

	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		// Assumes splitter always has arguments, and if an argument is an index, the first one is.
		boolean result = indices.contains(splitter.get(0));
		return result;
	}

	@Override
	public boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll() {
		return false;
	}

	// END OF GENERIC CODE
	
	// STATIC DECLARATIONS FOR USE IN Constraint (cannot be declared inside it because non-static inner classes cannot have static members).
	
	private static final Times timesRewriter = new Times(); // for use in the class below

	protected interface NonEqualityConstraints {
		NonEqualityConstraints clone();
	}

	@SuppressWarnings("serial")
	/**
	 * Represents and manipulates constraints in the equalityTheory of disequalities of terms (variables and constants).
	 */
	@Beta
	public abstract class Constraint extends AbstractTheory.AbstractConstraint {

		// The algorithm is based on the counting principle: to determine the model count, we
		// go over indices, in a certain order, and analyse how many possible values each one them has,
		// based on how many constants, free variables, and previous indices are constrained to be disequal from it.
		// (free variables and constants are considered less than indices in the choosing order).

		// Equalities define equivalence classes.
		// Disequalities are represented on equivalent classes representatives only.
		
		// A "disequal" of a variable V is a term T that comes *before* V in the choosing order.
		// This means that this word is being used in a non-symmetric way.
		// When we mean the symmetric sense of it, that is, "disequal according to the equalityTheory",
		// we say "constrained to be disequal".

		// We map each variable equivalent class representative (including free ones) to its set of disequals.
		
		// We use "distinct" to refer to non-equal Java objects
		// (as opposed to terms not being equal on the equality equalityTheory level).
		
		// Invariants:
		// Terms belong to equivalence classes depending on what equality splitters have been applied before.
		// Each equivalence class is represented *only* by its representative in the disequalities data structure (the map super class)
		// and arguments of generalized variables in the equalities.
		// If an equivalent class contains a constant, that constant must be its representative
		// (because it contains the extra implicit information about its disequality to other constants).
		// equalitiesMap maps variables to another term of its equivalence class.
		
		// The map (super class) keeps disequals.
		
		protected Map<Expression, Expression> equalitiesMap;
		protected LinkedHashMap<Expression, NonEqualityConstraints> nonEqualityConstraintsMap;
		
		public Constraint(Collection<Expression> indices) {
			super(indices);
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>();
			this.nonEqualityConstraintsMap = new LinkedHashMap<Expression, NonEqualityConstraints>(); 
		}

		protected Constraint(Constraint another) {
			super(another.indices);
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>(another.equalitiesMap);
			this.nonEqualityConstraintsMap = new LinkedHashMap<Expression, NonEqualityConstraints>(); 
			for (Map.Entry<Expression, NonEqualityConstraints> entry : another.nonEqualityConstraintsMap.entrySet()) {
				nonEqualityConstraintsMap.put(entry.getKey(), entry.getValue().clone()); // must copy sets to avoid interference. OPTIMIZATION: use a copy-as-needed implementation of set later.
			}
		}

		abstract public Constraint clone();

		/** Class an instance of which must be thrown when a contradiction is found during application of splitter. */
		protected class Contradiction extends Error {};

		/**
		 * Modify this constraint's inner representation to include this splitter.
		 */
		abstract protected void applyNormalizedSplitterDestructively(Expression splitter, RewritingProcess process);

		/**
		 * Modify this constraint's inner representation to include this splitter's negation.
		 */
		abstract protected void applyNormalizedSplitterNegationDestructively(Expression splitter, RewritingProcess process);

		/**
		 * Modify this constraint's inner representation to use up-to-date representatives.
		 */
		abstract protected void updateRepresentativesWhereverTheyAreUsed(RewritingProcess process);
		// OPTIMIZATION: can we provide the method above with the representatives that have been updated to minimize
		// representative lookup?

		/**
		 * Indicates whether two representatives are constrained to be disequal by this constraint.
		 */
		abstract protected boolean representativesAreExplicitlyConstrainedToBeDisequal(Expression representative1, Expression representative2, RewritingProcess process);
		
		/**
		 * Simplifies a given splitter to true if implied by constraint, false if its negation is implied by constraint,
		 * or a version of itself with terms replaced by representatives.
		 * Note that {@link #normalize(Expression, RewritingProcess)} cannot be used instead of this method
		 * because it transforms 'Term = true' into 'Term' and 'Term = false' into 'not Term',
		 * and these are not splitters for this theory anymore.
		 */
		@Override
		public
		abstract Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process);

		@Override
		abstract public Expression normalize(Expression expression, RewritingProcess process);
		
		@Override
		abstract public String toString();

		///////////////////////// CODE BELOW THIS LINE MUST BE GENERIC
		
		@Override
		public Collection<Expression> getIndices() {
			return indices;
		}

		/**
		 * Returns an expression (in the free variables) for the number of possible values for the given index,
		 * assuming that {@link #provideSplitterRequiredForComputingNumberOfValuesFor(Expression, RewritingProcess)}
		 * currently returns <code>null</code>,
		 * that is, we do not need anything splitters to be either imposed or negated in order to compute that.
		 */
		abstract protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process);

		@Override
		public Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
			Constraint result;

			Expression normalizedSplitterGivenConstraint = normalizeSplitterGivenConstraint(splitter, process);
			
			if (normalizedSplitterGivenConstraint.equals(splitterSign)) {
				result = this; // splitter is redundant given constraint
			}
			else if (normalizedSplitterGivenConstraint.equals( ! splitterSign)) {
				result = null; // splitter is contradictory given constraint
			}
			else {
				try {
					if (splitterSign) {
						result = applyNormalizedSplitter(normalizedSplitterGivenConstraint, process);
					}
					else {
						result = applyNormalizedSplitterNegation(normalizedSplitterGivenConstraint, process);
					}
				}
				catch (Contradiction e) {
					result = null;
				}
			}

			return result;
		}

		private Constraint applyNormalizedSplitter(Expression splitter, RewritingProcess process) {
			Constraint newConstraint = clone();
			newConstraint.applyNormalizedSplitterDestructively(splitter, process);
			return newConstraint;
		}

		private Constraint applyNormalizedSplitterNegation(Expression splitter, RewritingProcess process) {
			Constraint newConstraint = clone();
			newConstraint.applyNormalizedSplitterNegationDestructively(splitter, process);
			return newConstraint;
		}

		protected Expression computeModelCountGivenConditionsOnFreeVariables(RewritingProcess process) {
			List<Expression> numberOfPossibleValuesForIndicesSoFar = new LinkedList<Expression>();
			
			for (Expression index : indices) {
				if ( ! indexIsBound(index)) {
					Expression numberOfPossibleValuesForIndex = computeNumberOfPossibleValuesFor(index, process);
					numberOfPossibleValuesForIndicesSoFar.add(numberOfPossibleValuesForIndex);
				}
			}
			
			Expression result = Times.make(numberOfPossibleValuesForIndicesSoFar);
			Expression unconditionalCount = timesRewriter.rewrite(result, process);
			return unconditionalCount;
		}

		////////// EQUALITY CONSTRAINTS MAINTENANCE
		
		/**
		 * Indicates the binding of a variable in the equalities map.
		 * A chain of bindings always links terms constrained to be equal and its
		 * final element is the representative of the equivalent class of those terms.
		 * @param variable
		 * @return
		 */
		private Expression getBinding(Expression variable) {
			Expression result = equalitiesMap.get(variable);
			return result;
		}

		/**
		 * Modifies the binding of a variable to a new one
		 * (see {@link #getBinding(Expression)}).
		 */
		protected void setBinding(Expression variable, Expression newBinding) {
			if ( ! variable.equals(newBinding)) {
				equalitiesMap.put(variable, newBinding);
			}
		}

		/**
		 * Same as {@link #setBinding(Expression, Expression, RewritingProcess),
		 * but for an arbitrary equality map, as opposed to the constraint's.
		 */
		protected void setBinding(Map<Expression, Expression> equalitiesMap, Expression variable, Expression binding) {
			if ( ! variable.equals(binding)) {
				equalitiesMap.put(variable, binding);
			}
		}

		/**
		 * Indicates whether an index is bound to some other term.
		 */
		protected boolean indexIsBound(Expression index) {
			return equalitiesMap.containsKey(index);
		}

		/**
		 * A normalized term's representative is itself, if the term is a constant,
		 * the final term in the current binding chain, if the term is a variable and it has a binding,
		 * or itself if it is a variable without a binding.
		 * If the term is a variable with a binding,
		 * this method sets its binding to the final term in the chain
		 * for greater efficiency next time the method is invoked.
		 * @param term
		 * @param process
		 * @return
		 */
		protected Expression getRepresentative(Expression term, RewritingProcess process) {
			return getRepresentative(term, true /* record direct binding to representative */, process);
		}
		
		/**
		 * A normalized term's representative is itself, if the term is a constant,
		 * the final term in the current binding chain, if the term is a variable and it has a binding,
		 * or itself if it is a variable without a binding.
		 * If the term is a variable with a binding,
		 * this method sets its binding to the final term in the chain
		 * for greater efficiency next time the method is invoked.
		 * @param term
		 * @param process
		 * @return
		 */
		protected Expression getRepresentative(Expression term, boolean recordDirectBindingToRepresentative, RewritingProcess process) {
			Expression current = term;
			Expression currentBinding;
			while (termTheory.isVariableTerm(current, process) && (currentBinding = getBinding(current)) != null) {
				current = currentBinding;
			}
			// now, 'current' is in the chain started at term,
			// and it is either a constant or a variable without binding, therefore it is the equivalence class representative.
			if (recordDirectBindingToRepresentative && termTheory.isVariableTerm(term, process)) {
				setBinding(term, current); // optional recording so that we do not need to traverse the entire chain next time
			}
			return current;
		}
		
		protected boolean termsAreExplicitlyConstrainedToBeEqual(Expression variable, Expression otherTerm, RewritingProcess process) {
			boolean result = getRepresentative(variable, process).equals(getRepresentative(otherTerm, process));
			return result;
		}

		////////// END OF EQUALITY CONSTRAINTS MAINTENANCE
	}
}