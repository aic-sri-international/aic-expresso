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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.mapIntoSetOrSameIfNoDistinctElementInstances;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
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
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * A {@link Theory} for equality literals.
 */
public class EqualityTheory extends AbstractEqualityTheory {
	
	public TermTheory termTheory;
	
	// Important:
	// this class generalizes the notion of a variable to a "generalized variable" (simply referred by as "variable"),
	// which is either a variable symbol, or an uninterpreted function application such as p(a, b, X).
	// It can also be seen as an indexed variable (typically represented as x_i, y_i,j etc).
	
	public EqualityTheory(TermTheory termTheory) {
		super(termTheory);
		this.termTheory = termTheory;
	}

	private static Rewriter plus  = new Plus();
	private static Rewriter times = new Times();
	
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
	
	public static class DisequalitiesConstraints implements NonEqualityConstraints {
		private Collection<Expression> disequals = new LinkedHashSet<Expression>();
		
		public DisequalitiesConstraints() {
		}

		public DisequalitiesConstraints(NonEqualityConstraints another) {
			disequals = new LinkedHashSet<Expression>(((DisequalitiesConstraints) another).disequals);
		}
		
		Collection<Expression> getDisequals() {
			return Collections.unmodifiableCollection(disequals);
		}

		public void add(Expression term) {
			disequals.add(term);
		}
		
		public String toString() {
			return disequals.toString();
		}
		
		public DisequalitiesConstraints clone() {
			return new DisequalitiesConstraints(this);
		}
	}
	
	/**
	 * Represents and manipulates constraints in the theoryWithEquality of disequalities of terms (variables and constants).
	 */
	@Beta
	public class EqualityConstraint extends AbstractEqualityTheory.AbstractEqualityConstraint {

		// The algorithm is based on the counting principle: to determine the model count, we
		// go over supportedIndices, in a certain order, and analyse how many possible values each one them has,
		// based on how many constants, free variables, and previous supportedIndices are constrained to be disequal from it.
		// (free variables and constants are considered less than supportedIndices in the choosing order).

		// Equalities define equivalence classes.
		// Disequalities are represented on equivalent classes representatives only.
		
		// A "disequal" of a variable V is a term T that comes *before* V in the choosing order.
		// This means that this word is being used in a non-symmetric way.
		// When we mean the symmetric sense of it, that is, "disequal according to the theoryWithEquality",
		// we say "constrained to be disequal".

		// We map each variable equivalent class representative (including free ones) to its set of disequals.
		
		// We use "distinct" to refer to non-equal Java objects
		// (as opposed to terms not being equal on the equality theoryWithEquality level).
		
		// Invariants:
		// Terms belong to equivalence classes depending on what equality splitters have been applied before.
		// Each equivalence class is represented *only* by its representative in the disequalities data structure (the map super class)
		// and arguments of generalized variables in the equalities.
		// If an equivalent class contains a constant, that constant must be its representative
		// (because it contains the extra implicit information about its disequality to other constants).
		// equalitiesMap maps variables to another term of its equivalence class.
		
		// The map (super class) keeps disequals.
		
		public EqualityConstraint(Collection<Expression> supportedIndices) {
			super(supportedIndices);
		}

		private EqualityConstraint(EqualityConstraint another) {
			super(another);
		}

		@Override
		public EqualityConstraint clone() {
			return new EqualityConstraint(this);
		}

		@Override
		protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression x, RewritingProcess process) {
			Expression result;
			if (indexIsBound(x)) {
				result = null;
			}
			else {
				result = getSplitterTowardsEnsuringTotalDisequalityOfTermsInCollection(getDisequals(x), process);
			}
			return result;
		}

		private Expression getSplitterTowardsEnsuringTotalDisequalityOfTermsInCollection(Collection<Expression> terms, RewritingProcess process) {
			for (Expression y : terms) {
				if (isVariableTerm(y, process)) { // we can restrict y to variables because at least one of y or t must be a variable (otherwise they would be two constants and we already know those are disequal).
					Expression splitter = getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(y, terms, process); // TODO: search from y's position only
					if (splitter != null) {
						return splitter;
					}
				}
			}
			
			return null;
		}

		private Expression getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(Expression term, Collection<Expression> terms, RewritingProcess process) {
			for (Expression anotherTerm : terms) {
				if ( ! anotherTerm.equals(term)) {
					Expression splitter = termTheory.getSplitterTowardDisunifyingDistinctTerms(term, anotherTerm, process);
					if (splitter != null) {
						return splitter; // need to disunify first
					}
					else if ( ! termsAreExplicitlyConstrainedToBeDisequal(term, anotherTerm, process)) { // already disunified
						splitter = makeSplitterFromFunctorAndTwoTerms(FunctorConstants.EQUALITY, term, anotherTerm, supportedIndices, process);
						return splitter;
					}
				}
			}
			return null;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			Expression simplifiedSplitterGivenConstraint;
			Expression representative1 = getRepresentative(splitter.get(0), process);
			Expression representative2 = getRepresentative(splitter.get(1), process);
			simplifiedSplitterGivenConstraint = Equality.makeWithConstantSimplification(representative1, representative2, process);
			if ( ! simplifiedSplitterGivenConstraint.getSyntacticFormType().equals("Symbol")) {
				if (representativesAreExplicitlyConstrainedToBeDisequal(representative1, representative2, process)) {
					simplifiedSplitterGivenConstraint = FALSE;
				}
			}
			return simplifiedSplitterGivenConstraint;
		}

		@Override
		public Expression normalize(Expression expression, RewritingProcess process) {
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
			Expression variable  = splitter.get(0);
			Expression otherTerm = splitter.get(1);
			if (splitterSign) {
				applyRepresentativesEqualityDestructively(variable, otherTerm, process);
			}
			else {
				applyRepresentativesDisequalityDestructively(variable, otherTerm, process);
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
			updateRepresentativesWhereverTheyAreUsed(process);
		}

		@Override
		protected void updateRepresentativesWhereverTheyAreUsed(RewritingProcess process) {
			updateRepresentativesInEqualitiesMap(process);
			updateRepresentativesInDisequalitiesMap(process);
		}

		private void updateRepresentativesInEqualitiesMap(RewritingProcess process) {
			if ( ! termTheory.termsHaveNoArguments()) {
				boolean representativesWereUpdated;
				do {
					representativesWereUpdated = false;
					Map<Expression, Expression> newEqualitiesMap = new LinkedHashMap<Expression, Expression>();
					for (Expression variable : equalitiesMap.keySet()) {
						Expression oldRepresentative = getRepresentative(variable, false /* do not update map as we are iterating over it */, process);
						Expression newVariable = termTheory.normalizeTermInEquality(variable, this, process);

						Expression newRepresentative;
						Expression representativesEquality;
						if (newVariable == variable) {
							newRepresentative = oldRepresentative;
							representativesEquality = TRUE;
						}
						else {
							newRepresentative = getRepresentative(newVariable, false, process);
							representativesEquality = makeSplitterFromFunctorAndTwoTerms(FunctorConstants.EQUALITY, oldRepresentative, newRepresentative, supportedIndices, process);
							representativesEquality = simplify(representativesEquality, process);
							if (representativesEquality.equals(FALSE)) {
								throw new Contradiction();
							}
						}

						if (representativesEquality.equals(TRUE)) {
							setBinding(newEqualitiesMap, newVariable, newRepresentative);
						}
						else {
							representativesWereUpdated = true;
							setBinding(newEqualitiesMap, newVariable, newRepresentative);
							setBinding(newEqualitiesMap, representativesEquality.get(0), representativesEquality.get(1));
						}
					}
					equalitiesMap = representativesWereUpdated? newEqualitiesMap : equalitiesMap;
				} while (representativesWereUpdated);
			}
		}

		private void updateRepresentativesInDisequalitiesMap(RewritingProcess process) {
			Set<Expression> deletedKeys = new LinkedHashSet<Expression>();
			Map<Expression, Collection<Expression>> updatedDisequals = new LinkedHashMap<Expression, Collection<Expression>>();
			for (Map.Entry<Expression, NonEqualityConstraints> entry : nonEqualityConstraintsMap.entrySet()) {
				Expression variable = entry.getKey();
				Expression newVariable = useRepresentatives(variable, process);
				Collection<Expression> disequals = ((DisequalitiesConstraints) entry.getValue()).getDisequals();
				Collection<Expression> newDisequalsFromThisEntry = mapIntoSetOrSameIfNoDistinctElementInstances(disequals, e -> useRepresentatives(e, process));
				if (newDisequalsFromThisEntry.contains(newVariable)) {
					throw new Contradiction();
				}
				if (newVariable != variable || newDisequalsFromThisEntry != disequals) {
					Collection<Expression> totalDisequalsSoFarForNewVariable = updatedDisequals.get(newVariable); // a previous key may have already been modified to newVariable
					if (totalDisequalsSoFarForNewVariable == null) {
						totalDisequalsSoFarForNewVariable = newDisequalsFromThisEntry;
					}
					else {
						totalDisequalsSoFarForNewVariable.addAll(newDisequalsFromThisEntry);
					}
					updatedDisequals.put(newVariable, totalDisequalsSoFarForNewVariable);
					deletedKeys.add(variable);
				}
			}
			
			// now we update the disequalities map if needed
			if ( ! updatedDisequals.isEmpty()) {
				
				// we start by removing the modified entries
				for (Expression deletedKey : deletedKeys) {
					nonEqualityConstraintsMap.remove(deletedKey);
				}
				
				// and now we add the new disequalities. Note we cannot just put them in the map as they are, because of choosing order
				for (Map.Entry<Expression, Collection<Expression>> updatedEntry : updatedDisequals.entrySet()) {
					for (Expression disequal : updatedEntry.getValue()) {
						applyRepresentativesDisequalityDestructively(updatedEntry.getKey(), disequal, process);
					}
				}
			}
		}

		private Expression useRepresentatives(Expression key, RewritingProcess process) {
			Expression result = key.replaceAllOccurrences(t -> getRepresentative(t, process), process);
			return result;
		}

		/** Assumes disequality does not turn constraint into contradiction */
		private void applyRepresentativesDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
			if (termTheory.isVariableTerm(term1, process) || termTheory.isVariableTerm(term2, process)) {
				if (termTheory.isVariableTerm(term1, process) && variableIsChosenAfterOtherTerm(term1, term2, supportedIndices, process)) {
					addFirstTermAsDisequalOfSecondTerm(term1, term2);
				}
				else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
					addFirstTermAsDisequalOfSecondTerm(term2, term1);
				}
			}
			// else they are both constants, and distinct ones, so no need to do anything.
		}

		private void addFirstTermAsDisequalOfSecondTerm(Expression term1, Expression term2) {
			DisequalitiesConstraints disequalsOfTerm1 = (DisequalitiesConstraints)
					Util.getValuePossiblyCreatingIt(nonEqualityConstraintsMap, term1, DisequalitiesConstraints.class); // cannot use getDisequals(term1) here because that method does not create a new set if needed, but simply uses a constant empty collection. This prevents unnecessary creation of collections.
			disequalsOfTerm1.add(term2);
		}

		protected boolean useDefaultImplementationOfModelCountByOverridingGetSplittersToBeSatisfiedAndGetSplittersToBeNotSatisfied() {
			return true;
		}

		@Override
		protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Expression variable : equalitiesMap.keySet()) {
				if ( ! indicesSubSet.contains(variable)) {
					Expression representative = getRepresentative(variable, process);
					// Note that a free variable's representative is never an index, because
					// in splitters supportedIndices always come before free variables,
					// and that is the order of the binding.
					// A splitter with a free variable as the first term will always have another free variable
					// or a constant on the right-hand side.
					// This matters because the conditional model count has to be in terms of
					// free variables and constants only, never supportedIndices.
					if ( ! representative.equals(variable)) {
						Expression splitter = Expressions.apply(FunctorConstants.EQUALITY, variable, representative); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
			return result;
		}

		@Override
		protected Collection<Expression> getSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Map.Entry<Expression, NonEqualityConstraints> entry : nonEqualityConstraintsMap.entrySet()) {
				assert termTheory.isVariableTerm(entry.getKey(), process);
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					for (Expression disequal : ((DisequalitiesConstraints) entry.getValue()).getDisequals()) {
						Expression splitter = Expressions.apply(FunctorConstants.EQUALITY, variable, disequal); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
			return result;
		}

		@Override
		protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
			Expression numberOfPossibleValuesForIndex;
			if (indexIsBound(index)) {
				numberOfPossibleValuesForIndex = ONE;
			}
			else {
				long numberOfNonAvailableValues = getDisequals(index).size();
				long typeSize = GrinderUtil.getTypeCardinality(index, process);
				if (typeSize == -1) {
					Expression indexType = process.getContextualSymbolType(index);
					if (indexType == null) {
						indexType = new DefaultSyntacticFunctionApplication(FunctorConstants.TYPE, index);
					}
					Expression indexTypeCardinality = apply(CARDINALITY, indexType);
					numberOfPossibleValuesForIndex = Minus.make(indexTypeCardinality, Expressions.makeSymbol(numberOfNonAvailableValues));
				}
				else {
					numberOfPossibleValuesForIndex = makeSymbol(Math.max(0, typeSize - numberOfNonAvailableValues));
				}
			}
			return numberOfPossibleValuesForIndex;
		}

		public Collection<Expression> getDisequals(Expression variable) {
			Collection<Expression> result;
			DisequalitiesConstraints disequalitiesConstraints = (DisequalitiesConstraints) nonEqualityConstraintsMap.get(variable);
			if (disequalitiesConstraints != null) {
				result = ((DisequalitiesConstraints) disequalitiesConstraints).getDisequals();
			}
			else {
				result = emptyList();
			}
			return result;
		}

		private boolean termsAreExplicitlyConstrainedToBeDisequal(Expression term1, Expression term2, RewritingProcess process) {
			Expression representative1 = getRepresentative(term1, process);
			Expression representative2 = getRepresentative(term2, process);
			boolean result = representativesAreExplicitlyConstrainedToBeDisequal(representative1, representative2, process);
			return result;
		}

		@Override
		protected boolean representativesAreExplicitlyConstrainedToBeDisequal(Expression representative1, Expression representative2, RewritingProcess process) {
			boolean result = false;
			if (process.isUniquelyNamedConstant(representative1) && process.isUniquelyNamedConstant(representative2)) {
				result = ! representative1.equals(representative2);
			}
			else if (getDisequals(representative1).contains(representative2)) {
				result = true;
			}
			else if (getDisequals(representative2).contains(representative1)) {
				result = true;
			}
			return result;
		}

		@Override
		public String toString() {
			String result = "Indices: " + supportedIndices + ", equalities: " + equalitiesMap + ", disequalities: " + nonEqualityConstraintsMap.toString();
			return result; 
		}
	}
}