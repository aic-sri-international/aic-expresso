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
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
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
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.GreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.LessThan;
import com.sri.ai.grinder.library.number.LessThanOrEqualTo;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.AbstractConstraintTheory;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SimplifyLiteralGivenDisequality;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;
import com.sri.ai.grinder.plaindpll.problemtype.Tautologicality;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.collect.CopyOnWriteMap;
import com.sri.ai.util.collect.StackedHashMap;
@Beta
/** 
 * A {@link ConstraintTheory} for equality literals.
 */
public abstract class AbstractEqualityConstraintTheory extends AbstractConstraintTheory {

	public TermTheory termTheory;

	// Important:
	// this class generalizes the notion of a variable to a "generalized variable" (simply referred by as "variable"),
	// which is either a variable symbol, or an uninterpreted function application such as p(a, b, X).
	// It can also be seen as an indexed variable (typically represented as x_i, y_i,j etc).

	public AbstractEqualityConstraintTheory(TermTheory termTheory) {
		this.termTheory = termTheory;
	}

	public TermTheory getTermTheory() {
		return termTheory;
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
					(f.numberOfArguments() == 2? Minus.simplify(f) : f),

					FunctorConstants.LESS_THAN,                 (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					LessThan.simplify(f),

					FunctorConstants.LESS_THAN_OR_EQUAL_TO,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					LessThanOrEqualTo.simplify(f),

					FunctorConstants.GREATER_THAN,              (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					GreaterThan.simplify(f),

					FunctorConstants.GREATER_THAN_OR_EQUAL_TO,  (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					GreaterThanOrEqualTo.simplify(f)

					);

	private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					ForAll.SYNTACTIC_FORM_TYPE,                             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SGDPLLT(this, new Tautologicality())).rewrite(f, process),

					ThereExists.SYNTACTIC_FORM_TYPE,                        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new SGDPLLT(this, new Satisfiability())).rewrite(f, process)
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
		if (expression.hasFunctor(FunctorConstants.EQUALITY) || expression.hasFunctor(FunctorConstants.DISEQUALITY)) { // TODO: need to generalize to other types of constraint
			result = FunctorConstants.EQUALITY;
		}
		else {
			result = getCorrespondingSplitterFunctorOtherThanEqualityOrNull(expression); // TODO: add abstract method to be invoked here.
		}
		return result;
	}

	abstract protected String getCorrespondingSplitterFunctorOtherThanEqualityOrNull(Expression expression);

	@Override
	protected boolean usesDefaultImplementationOfSimplifyExpressionGivenSplitterByOverriddingGetSplitterApplier() {
		return true;
	}

	@Override
	protected Function<Expression, Expression> getSplitterApplier(boolean splitterSign, Expression splitter) {
		Function<Expression, Expression> result;
		if (splitter.hasFunctor(FunctorConstants.EQUALITY)) {
			Expression term1 = splitter.get(0);
			Expression term2 = splitter.get(1);
			if (splitterSign) {
				result = e -> e.equals(term1)? term2 : e;
			}
			else {
				result = new SimplifyLiteralGivenDisequality(term1, term2);
			}
		}
		else {
			result = getNonEqualitySplitterApplier(splitterSign, splitter);
		}
		return result;
	}
	
	abstract protected Function<Expression, Expression> getNonEqualitySplitterApplier(boolean splitterSign, Expression splitter);
	
	@Override
	public EqualityConstraintTheoryConstraint makeConstraint(Collection<Expression> indices) {
		NonEqualitiesConstraint nonEqualities = makeNonEqualitiesConstraint(indices);
		EqualityConstraintTheoryConstraint result = new EqualityConstraintTheoryConstraint(indices, nonEqualities);
		return result;
	}

	abstract protected NonEqualitiesConstraint makeNonEqualitiesConstraint(Collection<Expression> indices);

	@Override
	public boolean isVariableTerm(Expression term, RewritingProcess process) {
		return termTheory.isVariableTerm(term, process);
	}

	/** A faster implementation than the default implementation, since we know more about splitters here. */
	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
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
	@SuppressWarnings("serial")
	@Beta
	public class EqualityConstraintTheoryConstraint extends AbstractOwnRepresentationConstraint {

		protected EqualitiesConstraint equalities; // TODO: make EqualitiesConstraint an interface
		protected NonEqualitiesConstraint nonEqualities;

		public EqualityConstraintTheoryConstraint(Collection<Expression> supportedIndices, NonEqualitiesConstraint nonEqualities) {
			super(supportedIndices);
			this.equalities = new EqualitiesConstraint(getTheory(), getSupportedIndices());
			this.nonEqualities = nonEqualities; 
		}

		private EqualityConstraintTheoryConstraint(EqualityConstraintTheoryConstraint another) {
			super(another.getSupportedIndices());
			this.equalities = new EqualitiesConstraint(another.equalities);
			this.nonEqualities = (NonEqualitiesConstraint) another.nonEqualities.clone();
		}

		@Override
		public EqualityConstraintTheoryConstraint clone() {
			return new EqualityConstraintTheoryConstraint(this);
		}

		@Override
		public AbstractEqualityConstraintTheory getTheory() {
			return AbstractEqualityConstraintTheory.this;
		}

		public TermTheory getTermTheory() {
			return termTheory;
		}

		@Override
		public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
			// equalities do not require splitters for model counting, so we ask only the non-equalities.
			Expression result = nonEqualities.pickSplitter(indicesSubSet, process);
			return result;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			myAssert(() -> splitter.numberOfArguments() == 2, () -> (new Object(){}).getClass().getEnclosingMethod() + " currently operates on binary splitters only.");
			
			Expression result;
			Expression representative1 = equalities.getRepresentative(splitter.get(0), process);
			Expression representative2 = equalities.getRepresentative(splitter.get(1), process);
			
			Expression normalizedSplitter = apply(splitter.getFunctor(), representative1, representative2);
			Expression normalizedSplitterSimplification = getTheory().simplify(normalizedSplitter, process); // careful, this may not be a splitter itself because X = true is simplified to X
			if (Expressions.isBooleanSymbol(normalizedSplitterSimplification)) {
				result = normalizedSplitterSimplification;
			}
			else {
				result = nonEqualities.normalizeSplitterGivenConstraint(normalizedSplitter, process);
			}

			return result;
		}

		private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap;
		
		private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap() {
			if (syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap == null) {

				syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap = 
						new StackedHashMap<String, BinaryFunction<Expression, RewritingProcess, Expression>>(syntacticFormTypeSimplifiers);
				
				BinaryFunction<Expression, RewritingProcess, Expression> representativeReplacer = (s, p) -> equalities.getRepresentative(s, p);

				syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap.put("Symbol", representativeReplacer);
				
				if ( ! getTermTheory().termsHaveNoArguments()) {
					syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap.put("Function application", representativeReplacer);
				}
			}
			return syntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap;
		}
		
		@Override
		public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
			Expression result = DPLLUtil.simplify(expression, functionApplicationSimplifiers, getSyntacticFormTypeSimplifiersIncludingRepresentativesInThisConstraintMap(), process);
			return result;
		}

		@Override
		public void incorporateNonTrivialSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
			if (splitterSign) {
				Expression variable  = splitter.get(0);
				Expression otherTerm = splitter.get(1);
				applyEqualityDestructively(variable, otherTerm, process);
			}
			else {
				nonEqualities.incorporateNonTrivialSplitterDestructively(splitterSign, splitter, process); 
			}
		}

		private void applyEqualityDestructively(Expression variable, Expression otherTerm, RewritingProcess process) {
			equalities.setBinding(variable, otherTerm);
			updateRepresentativesWhereverTheyAreUsedDestructively(process);
		}

		protected void updateRepresentativesWhereverTheyAreUsedDestructively(RewritingProcess process) {
			equalities.updateRepresentativesInEqualitiesMap(process);
			Function<Expression, Expression> getRepresentative = t -> equalities.getRepresentative(t, process); // TODO: we should only pass the actually updated terms, instead of a function on them all.
			nonEqualities.updateRepresentativesDestructively(getRepresentative, process);
		}

		@Override
		public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> splittersToBeSatisfiedFromEqualities = new LinkedHashSet<Expression>();
			equalities.getSplittersToBeSatisfiedFromEqualities(indicesSubSet, splittersToBeSatisfiedFromEqualities, process);
			Collection<Expression> splittersFromEqualitiesNotYetSatisfied = DPLLUtil.keepSplittersUnsatisfiedByContextualConstraint(splittersToBeSatisfiedFromEqualities, process);
			Collection<Expression> notBoundIndices = filter(indicesSubSet, i -> !equalities.indexIsBound(i));
			Expression result = nonEqualities.modelCount(notBoundIndices, process);
			if ( ! result.equals(ZERO)) {
				for (Expression splitter : splittersFromEqualitiesNotYetSatisfied) {
					result = IfThenElse.make(splitter, result, ZERO);
				}
			}
			return result;
		}

		@Override
		public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
			boolean result;
			if (literal.hasFunctor(EQUALITY)) {
				result = equalities.directlyImpliesNonTrivialLiteral(literal, process);
			}
			else if (literal.hasFunctor(DISEQUALITY)) {
				// since non-equalities are defined on representatives only, we need to find them first before delegating.
				Expression representative1 = equalities.getRepresentative(literal.get(0), process);
				Expression representative2 = equalities.getRepresentative(literal.get(1), process);
				result = nonEqualities.directlyImpliesDisequality(representative1, representative2, process);
			}
			else {
				// since non-equalities are defined on representatives only, we need to find them first before delegating.
				Expression representative1 = equalities.getRepresentative(literal.get(0), process);
				Expression representative2 = equalities.getRepresentative(literal.get(1), process);
				Expression representativesLiteral = apply(literal.getFunctor(), representative1, representative2);
				result = nonEqualities.directlyImpliesNonTrivialLiteral(representativesLiteral, process);
			}
			return result;
		}

		@Override
		protected Expression computeInnerExpression() {
			List<Expression> conjuncts = new LinkedList<Expression>();
			conjuncts.addAll(And.getConjuncts(equalities)); // using getConjuncts to ensure a final flat conjunction instead of a conjunction of two conjunctions
			conjuncts.addAll(And.getConjuncts(nonEqualities));
			Expression result = And.make(conjuncts);
			return result;
		}
	}

	/**
	 * Represents and manipulates constraints in the theoryWithEquality of disequalities of terms (variables and constants).
	 */
	@Beta
	public static class EqualitiesConstraint extends AbstractOwnRepresentationConstraint {

		private static final long serialVersionUID = 1L;

		public Map<Expression, Expression> equalitiesMap;
		protected AbstractEqualityConstraintTheory theory;

		public EqualitiesConstraint(AbstractEqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
			super(supportedIndices);
			this.theory = theory;
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>();
		}

		private EqualitiesConstraint(EqualitiesConstraint another) {
			super(another.getSupportedIndices());
			this.theory = another.theory;
			this.equalitiesMap = new CopyOnWriteMap<Expression, Expression>(another.equalitiesMap);
		}

		@Override
		public EqualitiesConstraint clone() {
			return new EqualitiesConstraint(this);
		}

		@Override
		public AbstractEqualityConstraintTheory getTheory() {
			return theory;
		}

		public TermTheory getTermTheory() {
			return getTheory().getTermTheory();
		}

		@Override
		public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
			myAssert( () -> literal.hasFunctor(EQUALITY), () -> "EqualitiesConstraint.directlyImplies must take equality *atoms* only.");
			boolean result1 = getRepresentative(literal.get(0), process).equals(getRepresentative(literal.get(1), process));
			boolean result = result1;
			return result;
		}

		@Override
		public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
			return null;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			Expression simplifiedSplitterGivenConstraint;
			Expression representative1 = getRepresentative(splitter.get(0), process);
			Expression representative2 = getRepresentative(splitter.get(1), process);
			simplifiedSplitterGivenConstraint = Equality.makeWithConstantSimplification(representative1, representative2, process);
			return simplifiedSplitterGivenConstraint;
		}

		@Override
		public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
			String syntacticTypeForm = "Symbol";
			BinaryFunction<Expression, RewritingProcess, Expression> representativeReplacer =
					(BinaryFunction<Expression, RewritingProcess, Expression>) (s, p) -> getRepresentative(s, p);

					Expression result = DPLLUtil.simplifyWithExtraSyntacticFormTypeSimplifiers(
							expression,
							theory.getFunctionApplicationSimplifiers(),
							theory.getSyntacticFormTypeSimplifiers(),
							process, syntacticTypeForm,
							representativeReplacer);

					return result;
		}

		@Override
		public void incorporateNonTrivialSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
			myAssert(() -> splitterSign, () -> "EqualitiesConstraint.incorporateNonTrivialSplitterDestructively must take positive splitters only."); 
			Expression variable  = splitter.get(0);
			Expression otherTerm = splitter.get(1);
			applyRepresentativesEqualityDestructively(variable, otherTerm, process);
		}

		private void applyRepresentativesEqualityDestructively(Expression variableRepresentative, Expression otherTermRepresentative, RewritingProcess process) {
			setBinding(variableRepresentative, otherTermRepresentative);
			updateRepresentativesInEqualitiesMap(process);
		}

		private void updateRepresentativesInEqualitiesMap(RewritingProcess process) {
			if ( ! getTermTheory().termsHaveNoArguments()) { // if terms have no arguments, simply keeping the bindings is enough;
				boolean equalitiesMapHasBeenUpdated;
				do {
					equalitiesMapHasBeenUpdated = false;
					Map<Expression, Expression> newEqualitiesMap = new LinkedHashMap<Expression, Expression>();
					for (Expression variable : equalitiesMap.keySet()) { // TODO: use the representatives that we know to have been updated, and go where they are instead of scanning the entire map
						Expression representativeOfVariable = getRepresentative(variable, false /* do not update map as we are iterating over it */, process);
						Expression newVariable = getTermTheory().normalizeTermModuloRepresentatives(variable,  t -> getRepresentative(t, process), process);

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
					// free variables and constants only, never indices, so representative cannot be an index.
					// FIXED BUG: however, variable may be a supported index that is not in the indicesSubSet! So we need to check representative not to be in indicesSubSet!
					if ( ! representative.equals(variable) &&  ! indicesSubSet.contains(representative)) {
						Expression splitter = apply(EQUALITY, variable, representative); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
		}

		@Override
		public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
			throw new Error("modelCount not yet implemented for EqualitiesConstraint"); // we don't need it for EqualityConstraintTheoryConstraint.modelCount (see that method to see why).
		}

		@Override
		protected Expression computeInnerExpression() {
			List<Expression> conjuncts = new LinkedList<Expression>();
			for (Map.Entry<Expression, Expression> entry : equalitiesMap.entrySet()) {
				conjuncts.add(Equality.make(entry.getKey(), entry.getValue()));
			}
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
		 * Indicates whether an variable is separator to some other term.
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

		private boolean isVariableTerm(Expression term, RewritingProcess process) {
			return getTheory().isVariableTerm(term, process);
		}

		////////// END OF EQUALITY CONSTRAINTS MAINTENANCE
	}
}