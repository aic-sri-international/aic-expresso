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
package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.filter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.AbstractTheory;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SimplifyLiteralGivenDisequality;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;
import com.sri.ai.grinder.plaindpll.problemtype.Tautologicality;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
@Beta
/** 
 * A {@link Theory} for equality literals.
 */
public class EqualityTheory extends AbstractTheory {
	
	public TermTheory termTheory;
	
	// Important:
	// this class generalizes the notion of a variable to a "generalized variable" (simply referred by as "variable"),
	// which is either a variable symbol, or an uninterpreted function application such as p(a, b, X).
	// It can also be seen as an indexed variable (typically represented as x_i, y_i,j etc).
	
	public EqualityTheory(TermTheory termTheory) {
		this.termTheory = termTheory;
	}

	private static Rewriter plus  = new Plus();
	private static Rewriter times = new Times();
	
	@Override
	protected boolean usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers() {
		return true;
	}

	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					FunctorConstants.EQUALITY,        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equality.simplify(f, process),
					
					FunctorConstants.DISEQUALITY,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.AND,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					And.simplify(f),

					FunctorConstants.OR,              (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Or.simplify(f),

					FunctorConstants.NOT,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.IF_THEN_ELSE,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					IfThenElse.simplify(f),

					FunctorConstants.EQUIVALENCE,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equivalence.simplify(f),

					FunctorConstants.IMPLICATION,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Implication.simplify(f),

					FunctorConstants.CARDINALITY,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					{ Expression type = (Expression) process.getGlobalObject(f); return type == null? f : type; },

					FunctorConstants.TIMES,           (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					times.rewrite(f, process),

					FunctorConstants.DIVISION,        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Division.simplify(f),

					FunctorConstants.PLUS,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					plus.rewrite(f, process),

					FunctorConstants.MINUS,           (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					f.numberOfArguments() == 2? Minus.simplify(f) : f
	);
	
	private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					ForAll.SYNTACTIC_FORM_TYPE,                             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SGDPLLT(new EqualityTheory(termTheory), new Tautologicality())).rewrite(f, process),
 
					ThereExists.SYNTACTIC_FORM_TYPE,                        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SGDPLLT(new EqualityTheory(termTheory), new Satisfiability())).rewrite(f, process)
	);

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	@Override
	protected String getCorrespondingSplitterFunctorOrNull(Expression expression) {
		String result;
		if (expression.hasFunctor(FunctorConstants.EQUALITY) || expression.hasFunctor(FunctorConstants.DISEQUALITY)) {
			result = FunctorConstants.EQUALITY;
		}
		else {
			result = null;
		}
		return result;
	}
	
	@Override
	protected BinaryFunction<Expression, RewritingProcess, Expression>
	getSplitterApplier(boolean splitterSign, Expression splitter) {
		BinaryFunction<Expression, RewritingProcess, Expression> applier;
		if (splitter.hasFunctor(FunctorConstants.EQUALITY)) {
			Expression term1 = splitter.get(0);
			Expression term2 = splitter.get(1);
			if (splitterSign) {
				applier = (Expression expression, RewritingProcess process) -> {
					Expression result = expression.replaceAllOccurrences(term1, term2, process);
					result = simplify(result, process);
					return result;
				};
			}
			else {
				applier = (Expression expression, RewritingProcess process) -> {
					Expression result = expression.replaceAllOccurrences(new SimplifyLiteralGivenDisequality(term1, term2), process);
					result = simplify(result, process);
					return result;
				};
			}
		}
		else {
			applier = null;
		}
		return applier;
	}
	
	@Override
	public EqualityConstraint makeConstraint(Collection<Expression> indices) {
		return new EqualityConstraint(indices);
	}
	
	@Override
	public boolean isVariableTerm(Expression term, RewritingProcess process) {
		return termTheory.isVariableTerm(term, process);
	}

	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		// A faster implementation than the default implementation, since we know more about splitters here.
		boolean result = indices.contains(splitter.get(0));
		return result;
	}

	@Override
	public boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll() {
		return false;
	}

	/**
	 * Represents and manipulates constraints in the theoryWithEquality of disequalities of terms (variables and constants).
	 */
	@Beta
	public class EqualityConstraint extends AbstractOwnRepresentationConstraint {

		private static final long serialVersionUID = 1L;

		public Map<Expression, Expression> equalitiesMap;
		public Constraint nonEqualitiesConstraint;

		public EqualityConstraint(Collection<Expression> indices) {
			super(indices);
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>();
			this.nonEqualitiesConstraint = new NonEqualitiesConstraint(supportedIndices, this); 
		}

		private EqualityConstraint(EqualityConstraint another) {
			super(another.getSupportedIndices());
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>(another.equalitiesMap); // TODO: implement a copy-on-write scheme
			this.nonEqualitiesConstraint = another.nonEqualitiesConstraint.copyWithNewParent(this);
		}

		@Override
		public EqualityConstraint copyWithNewParent(Constraint parentConstraint) {
			return (EqualityConstraint) super.copyWithNewParent(parentConstraint);
		}
		
		@Override
		public EqualityConstraint clone() {
			return new EqualityConstraint(this);
		}

		@Override
		public EqualityTheory getTheory() {
			return EqualityTheory.this;
		}

		public TermTheory getTermTheory() {
			return termTheory;
		}

		@Override
		public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Expression result = nonEqualitiesConstraint.pickSplitter(indicesSubSet, process);
			return result;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			Expression simplifiedSplitterGivenConstraint;
			Expression representative1 = getRepresentative(splitter.get(0), process);
			Expression representative2 = getRepresentative(splitter.get(1), process);
			simplifiedSplitterGivenConstraint = Equality.makeWithConstantSimplification(representative1, representative2, process);
			if ( ! simplifiedSplitterGivenConstraint.getSyntacticFormType().equals("Symbol")) {
				if (nonEqualitiesConstraint.directlyImplies(apply(DISEQUALITY, representative1, representative2), process)) { // TODO: create specialized interface to avoid this unnecessary creation of a literal
					simplifiedSplitterGivenConstraint = FALSE;
				}
			}
			return simplifiedSplitterGivenConstraint;
		}

		@Override
		public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
			String syntacticTypeForm = "Symbol";
			BinaryFunction<Expression, RewritingProcess, Expression> representativeReplacer =
					(BinaryFunction<Expression, RewritingProcess, Expression>) (s, p) -> getRepresentative(s, p);
		
			Expression result = DPLLUtil.simplifyWithExtraSyntacticFormTypeSimplifier(
					expression,
					functionApplicationSimplifiers,
					syntacticFormTypeSimplifiers,
					syntacticTypeForm, representativeReplacer,
					process);
			
			return result;
		}

		@Override
		protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
			if (splitterSign) {
				Expression variable  = splitter.get(0);
				Expression otherTerm = splitter.get(1);
				applyRepresentativesEqualityDestructively(variable, otherTerm, process);
			}
			else {
				nonEqualitiesConstraint = nonEqualitiesConstraint.incorporatePossiblyDestructively(splitterSign, splitter, process); 
			}
		}

		private void applyRepresentativesEqualityDestructively(Expression variableRepresentative, Expression otherTermRepresentative, RewritingProcess process) {
			// To apply the equality of these two representatives we must take several steps:
			// first, we include the binding from the first to the second.
			// This means the first term ceases being a representative, and is now represented by the second term.
			// We call such a term an obsolete representative.
			// We have a set of obsolete representatives, initially containing only this one obsolete representative,
			// which must be replaced everywhere a representative is used by its new representative.
			// These places are the representations of bound terms and the disequals map.
			// To replace an obsolete representative, we first replace it in the disequals map.
			// This may lead to finding a contradiction, if a disequal becomes equal to the term
			// it is constrained to be disequal from.
			// We then replace it in the bound terms representations.
			// For each bound term T, let R be the representative of T and R' the representative of updated term T'.
			// If R and R' are distinct constants, fail.
			// If they are the same term, link T' to R' if it is distinct from R'.
			// Otherwise, bind the variable among R, R' to the other.
			// If T is a representative itself, it is added to the set of obsolete representatives.
			// Once the first updating is finished, we pick another of the recorded obsolete representatives and repeat till they are all updated.
			// Note that when it is time for an obsolete representative to be updated, its parts may have been updated in the meantime,
			// so it is important to update its part before proceeding.
			//
			// OR... to keep things simple if a bit less optimized...
			//
			// We simply keep updating everything according to the latest representatives until there are no changes.
			// We go over disequals map and update each term, checking whether disequalities are violated.
			// Then we go over bound items, check if its representatives before or after updating are distinct,
			// do nothing if they are the same, fail if they are distinct constants,
			// link variable to the other one otherwise while setting flag indicating representative change,
			// keep going until the flag does not change.

			setBinding(variableRepresentative, otherTermRepresentative);
			updateRepresentativesWhereverTheyAreUsedDestructively(process);
		}

		protected void updateRepresentativesWhereverTheyAreUsedDestructively(RewritingProcess process) {
			updateRepresentativesInEqualitiesMap(process);
			Function<Expression, Expression> getRepresentative = t -> getRepresentative(t, process); // TODO: we should only pass the actually updated terms, instead of a function on them all.
			nonEqualitiesConstraint = nonEqualitiesConstraint.updateRepresentativesPossiblyDestructively(getRepresentative, process);
		}

		private void updateRepresentativesInEqualitiesMap(RewritingProcess process) {
			if ( ! termTheory.termsHaveNoArguments()) {
				boolean equalitiesMapHasBeenUpdated;
				do {
					equalitiesMapHasBeenUpdated = false;
					Map<Expression, Expression> newEqualitiesMap = new LinkedHashMap<Expression, Expression>();
					for (Expression variable : equalitiesMap.keySet()) {
						Expression representativeOfVariable = getRepresentative(variable, false /* do not update map as we are iterating over it */, process);
						Expression newVariable = termTheory.normalizeTermInEquality(variable, this, process);

						if (newVariable == variable) {
							// no change for this variable, just copy this entry to the new map
							setBinding(newEqualitiesMap, variable, representativeOfVariable);
						}
						else {
							Expression representativeOfNewVariable = getRepresentative(newVariable, false, process);
							Expression representativesEquality = Equality.makeWithConstantSimplification(representativeOfVariable, representativeOfNewVariable, process);
							if (representativesEquality.equals(FALSE)) {
								throw new Contradiction();
							}
							else {
								setBinding(newEqualitiesMap, newVariable, representativeOfNewVariable);
								if ( ! representativesEquality.equals(TRUE)) {
									// variable = representativeOfVariable and variable = newVariable = representativeOfNewVariable,
									// therefore, by transitivity, representativeOfVariable = representativeOfNewVariable
									setBinding(newEqualitiesMap, representativeOfVariable, representativeOfNewVariable);
								}
								equalitiesMapHasBeenUpdated = true;
							}
						}
					}
					equalitiesMap = equalitiesMapHasBeenUpdated? newEqualitiesMap : equalitiesMap;
				} while (equalitiesMapHasBeenUpdated);
			}
		}

		public void getSplittersToBeSatisfiedFromEqualities(Collection<Expression> indicesSubSet, Collection<Expression> result, RewritingProcess process) {
			for (Expression variable : equalitiesMap.keySet()) {
				if ( ! indicesSubSet.contains(variable)) {
					Expression representative = getRepresentative(variable, process);
					// Note that a free variable's representative is never a supported index, because
					// in splitters indices always come before free variables,
					// and that is the order of the binding.
					// A splitter with a free variable as the first term will always have another free variable
					// or a constant on the right-hand side.
					// This matters because the conditional model count has to be in terms of
					// free variables and constants only, never indices.
					if ( ! representative.equals(variable)) {
						Expression splitter = apply(EQUALITY, variable, representative); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
		}

		@Override
		public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> splittersToBeSatisfiedFromEqualities = new LinkedHashSet<Expression>();
			getSplittersToBeSatisfiedFromEqualities(indicesSubSet, splittersToBeSatisfiedFromEqualities, process);
			Collection<Expression> splittersFromEqualitiesNotYetSatisfied = DPLLUtil.keepSplittersUnsatisfiedByContextualConstraint(splittersToBeSatisfiedFromEqualities, process);
			Collection<Expression> notBoundIndices = filter(indicesSubSet, i -> !indexIsBound(i));
			Expression result = nonEqualitiesConstraint.modelCount(notBoundIndices, process);
			if ( ! result.equals(ZERO)) {
				for (Expression splitter : splittersFromEqualitiesNotYetSatisfied) {
					result = IfThenElse.make(splitter, result, ZERO);
				}
			}
			return result;
		}
		
		public boolean termsAreExplicitlyConstrainedToBeDisequal(Expression term1, Expression term2, RewritingProcess process) {
			Expression representative1 = getRepresentative(term1, process);
			Expression representative2 = getRepresentative(term2, process);
			boolean result = nonEqualitiesConstraint.directlyImplies(apply(DISEQUALITY, representative1, representative2), process); // TODO: create specialized interface to avoid this unnecessary creation of a literal
			return result;
		}

		@Override
		protected Expression computeInnerExpression() {
			List<Expression> conjuncts = new LinkedList<Expression>();
			for (Map.Entry<Expression, Expression> entry : equalitiesMap.entrySet()) {
				conjuncts.add(Equality.make(entry.getKey(), entry.getValue()));
			}
			conjuncts.add(nonEqualitiesConstraint);
			Expression result = And.make(conjuncts);
			return result;
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
		 * Indicates whether an variable is bound to some other term.
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
		public Expression getRepresentative(Expression term, RewritingProcess process) {
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
			while (isVariableTerm(current, process) && (currentBinding = getBinding(current)) != null) {
				current = currentBinding;
			}
			// now, 'current' is in the chain started at term,
			// and it is either a constant or a variable without binding, therefore it is the equivalence class representative.
			if (recordDirectBindingToRepresentative && isVariableTerm(term, process)) {
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