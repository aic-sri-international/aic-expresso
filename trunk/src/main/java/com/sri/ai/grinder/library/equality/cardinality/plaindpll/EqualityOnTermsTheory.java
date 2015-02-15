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
import java.util.LinkedList;
import java.util.List;
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
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * A {@link Theory} for equality literals.
 */
public class EqualityOnTermsTheory extends AbstractTheory {
	
	public TermTheory termTheory;
	
	// Important:
	// this class generalizes the notion of a variable to a "generalized variable" (simply referred by as "variable"),
	// which is either a variable symbol, or an uninterpreted function application such as p(a, b, X).
	// It can also be seen as an indexed variable (typically represented as x_i, y_i,j etc).
	
	public EqualityOnTermsTheory(TermTheory termTheory) {
		super();
		this.termTheory = termTheory;
	}

	private static Rewriter plus  = new Plus();
	private static Rewriter times = new Times();
	
	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					FunctorConstants.EQUALITY,       (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equality.simplify(f, process),
					
					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.DISEQUALITY,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.AND,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					And.simplify(f),

					FunctorConstants.OR,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Or.simplify(f),

					FunctorConstants.NOT,            (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.IF_THEN_ELSE,   (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					IfThenElse.simplify(f),

					FunctorConstants.EQUIVALENCE,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equivalence.simplify(f),

					FunctorConstants.IMPLICATION,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Implication.simplify(f),

					FunctorConstants.TIMES,          (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					times.rewrite(f, process),

					FunctorConstants.PLUS,           (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					plus.rewrite(f, process)
	);
	
	private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					ForAll.SYNTACTIC_FORM_TYPE,                             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new DPLLGeneralizedAndSymbolic(new EqualityOnTermsTheory(termTheory), new Tautologicality())).rewrite(f, process),
 
					ThereExists.SYNTACTIC_FORM_TYPE,                        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					(new DPLLGeneralizedAndSymbolic(new EqualityOnTermsTheory(termTheory), new Satisfiability())).rewrite(f, process)
	);

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	///////////// MAKE ABSTRACT IN NEW CLASS
	
	/**
	 * If expression can generate a splitter, returns the appropriate splitter's functor;
	 * otherwise, returns <code>null</code>.
	 * @param expression
	 * @return
	 */
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
	
	/**
	 * @param splitterFunctor
	 * @return
	 */
	private BinaryFunction<Expression, Expression, Expression> getSplitterMaker(String splitterFunctor) {
		BinaryFunction<Expression, Expression, Expression> result =
				splitterFunctor.equals(FunctorConstants.EQUALITY)? (t1, t2) -> Equality.make(t1, t2) : null;
		return result;
	}

	protected BinaryFunction<Expression, RewritingProcess, Expression>
	
	getSplitterApplier(Expression splitter) {

		BinaryFunction<Expression, RewritingProcess, Expression> applier = null;

		if (splitter.hasFunctor(FunctorConstants.EQUALITY)) { // TODO: make this if then else into a map from functors for efficiency
			applier = (Expression expression, RewritingProcess process) -> {
				Expression term1 = splitter.get(0);
				Expression term2 = splitter.get(1);
				Expression result = expression.replaceAllOccurrences(term1, term2, process);
				result = simplify(result, process);
				return result;
			};
		}

		return applier;
	}

	protected BinaryFunction<Expression, RewritingProcess, Expression>
	
	getSplitterNegationApplier(Expression splitter) {
	
		BinaryFunction<Expression, RewritingProcess, Expression> applier = null;

		if (splitter.hasFunctor(FunctorConstants.EQUALITY)) { // TODO: make this if then else into a map from functors for efficiency
			applier = (Expression expression, RewritingProcess process) -> {
				Expression term1 = splitter.get(0);
				Expression term2 = splitter.get(1);
				Expression result = expression.replaceAllOccurrences(new SimplifyLiteralGivenDisequality(term1, term2), process);
				result = simplify(result, process);
				return result;
			};
		}

		return applier;
	}

	@Override
	public boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll() {
		return false;
	}

	@Override
	public Constraint makeConstraint(Collection<Expression> indices) {
		return new Constraint(indices);
	}
	
	///////////// END OF MAKE ABSTRACT IN NEW CLASS

	// FROM NOW ON CODE MUST BE GENERIC
	
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
					e -> termTheory.isVariableTerm(e, process));
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

	/**
	 * Makes splitter by applying given functor to two terms, indices coming first if any.
	 * Does not simplify splitter (so, if it is simplifiable, it does not get simplified).
	 * @param splitterFunctor the splitter's functor
	 * @param term1
	 * @param term2
	 * @param indices
	 * @param process
	 * @return
	 */
	protected Expression makeSplitterFromFunctorAndTwoTerms(String splitterFunctor, Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		BinaryFunction<Expression, Expression, Expression> maker = getSplitterMaker(splitterFunctor);
		Expression result;
		// Places index or variable before constants.
		if (indices.contains(term1)) {
			result = maker.apply(term1, term2);
		}
		else if (indices.contains(term2)) {
			result = maker.apply(term2, term1);
		}
		else if (termTheory.isVariableTerm(term1, process)) {
			result = maker.apply(term1, term2);
		}
		else {
			result = maker.apply(term2, term1);
		}
		return result;
	}

	@Override
	public Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process) {
		if ( ! splitterSign) {
			return applySplitterNegationToExpression(splitter, expression, process);
		}
	
		Expression result = getSplitterApplier(splitter).apply(expression, process);
		
		return result;
	}

	private Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = getSplitterNegationApplier(splitter).apply(expression, process);
		return result;
	}

	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		// Assumes splitter always has arguments, and if an argument is an index, the first one is.
		boolean result = indices.contains(splitter.get(0));
		return result;
	}

	/**
	 * Indicates whether variable is chosen after otherTerm in model counting choosing ordering.
	 */
	private static boolean variableIsChosenAfterOtherTerm(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isUniquelyNamedConstant(otherTerm) || variableIsChosenAfterOtherVariable(otherTerm, variable, indices);
		return result;
	}

	/**
	 * Indicates whether variable in chosen after otherVariable in choosing ordering.
	 */
	private static boolean variableIsChosenAfterOtherVariable(Expression variable, Expression otherVariable, Collection<Expression> indices) {
		boolean result;
		if (indices.contains(variable)) { // index
			if ( ! indices.contains(otherVariable)) { // free variable
				result = false; // free variables always precedes indices
			}
			else { // both are indices
				result = otherVariable.toString().compareTo(variable.toString()) < 0; // indices are compared alphabetically
			}
		}
		else if (indices.contains(otherVariable)) { // variable is free variable and otherVariable is index
			result = true; // free variable always precedes indices
		}
		else { // neither is index
			result = otherVariable.toString().compareTo(variable.toString()) < 0;	// alphabetically		
		}
		return result;
	}

	protected Expression makeTrueFalseOrSplitterFromTwoTerms(String splitterFunctor, Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		Expression result = makeSplitterFromFunctorAndTwoTerms(splitterFunctor, term1, term2, indices, process);
		result = simplify(result, process);
		return result;
	}

	private static final Times timesRewriter = new Times(); // for use in the class below

	private static interface NonEqualityConstraints {
		
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
	}
	
	@SuppressWarnings("serial")
	/**
	 * Represents and manipulates constraints in the equalityTheory of disequalities of terms (variables and constants).
	 */
	@Beta
	public class Constraint implements Theory.Constraint {

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
		
		public Constraint(Collection<Expression> indices) {
			super();
			this.indices = indices;
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>();
			this.nonEqualityConstraintsMap = new LinkedHashMap<Expression, NonEqualityConstraints>(); 
		}

		private Constraint(Constraint another) {
			this.indices = another.indices;
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>(another.equalitiesMap);
			this.nonEqualityConstraintsMap = new LinkedHashMap<Expression, NonEqualityConstraints>(); 
			for (Map.Entry<Expression, NonEqualityConstraints> entry : another.nonEqualityConstraintsMap.entrySet()) {
				nonEqualityConstraintsMap.put(entry.getKey(), new DisequalitiesConstraints(entry.getValue())); // must copy sets to avoid interference. OPTIMIZATION: use a copy-as-needed implementation of set later.
			}
		}

		/**
		 * @param x
		 * @param process
		 * @return
		 */
		protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression x, RewritingProcess process) {
			return getSplitterTowardsEnsuringTotalDisequalityOfTermsInCollection(getDisequals(x), process);
		}

		private Expression getSplitterTowardsEnsuringTotalDisequalityOfTermsInCollection(Collection<Expression> terms, RewritingProcess process) {
			for (Expression y : terms) {
				if (termTheory.isVariableTerm(y, process)) { // we can restrict y to variables because at least one of y or t must be a variable (otherwise they would be two constants and we already know those are disequal).
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
						splitter = makeSplitterFromFunctorAndTwoTerms(FunctorConstants.EQUALITY, term, anotherTerm, indices, process);
						return splitter;
					}
				}
			}
			return null;
		}

		/**
		 * @param splitter
		 * @param process
		 * @return
		 */
		private Expression simplifySplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			// THIS METHOD SHOULD PROBABLY BE MERGED WITH NORMALIZE
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

		/**
		 * @param splitter
		 * @param process
		 * @param newConstraint
		 */
		protected void applySplitterDestructively(Expression splitter, RewritingProcess process) {
			Expression variable  = splitter.get(0);
			Expression otherTerm = splitter.get(1);
			applyEqualityDestructively(variable, otherTerm, process);
		}

		/**
		 * @param splitter
		 * @param process
		 */
		private void applySplitterNegationDestructively(Expression splitter, RewritingProcess process) {
			Expression variable  = splitter.get(0);
			Expression otherTerm = splitter.get(1);
			applyDisequalityDestructively(variable, otherTerm, process);
		}

		private void applyEqualityDestructively(Expression variableRepresentative, Expression otherTermRepresentative, RewritingProcess process) {
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

			setBinding(variableRepresentative, otherTermRepresentative, process);
			updateRepresentativesWhereverTheyAreUsed(process);
		}

		private void updateRepresentativesWhereverTheyAreUsed(RewritingProcess process) {
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
							representativesEquality = makeTrueFalseOrSplitterFromTwoTerms(FunctorConstants.EQUALITY, oldRepresentative, newRepresentative, indices, process);
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
						applyDisequalityDestructively(updatedEntry.getKey(), disequal, process);
					}
				}
			}
		}

		private Expression useRepresentatives(Expression key, RewritingProcess process) {
			Expression result = key.replaceAllOccurrences(t -> getRepresentative(t, process), process);
			return result;
		}

		/** Assumes disequality does not turn constraint into contradiction */
		private void applyDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
			if (termTheory.isVariableTerm(term1, process) || termTheory.isVariableTerm(term2, process)) {
				if (termTheory.isVariableTerm(term1, process) && variableIsChosenAfterOtherTerm(term1, term2, indices, process)) {
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

		///////////////////////// CODE BELOW THIS LINE MUST BE GENERIC
		
		private Collection<Expression> indices;
		private Map<Expression, Expression> equalitiesMap;
		private LinkedHashMap<Expression, NonEqualityConstraints> nonEqualityConstraintsMap;
		
		@Override
		public Collection<Expression> getIndices() {
			return indices;
		}

		@Override
		public Expression pickSplitter(RewritingProcess process) {
		
			// if there is an index X such that X has disequals Y and T,
			// we must know if Y and T are either the same or disequal before we can tell
			// how many possible values X has.
		
			for (Expression x : nonEqualityConstraintsMap.keySet()) {
				if (indices.contains(x)) {
					if ( ! indexIsBound(x)) { // optional, but more efficient
						Expression splitter = provideSplitterRequiredForComputingNumberOfValuesFor(x, process);
						if (splitter != null) {
							return splitter;
						}
					}
				}
			}

			return null;
		}

		private Expression getBinding(Expression variable) {
			Expression result = equalitiesMap.get(variable);
			return result;
		}

		private void setBinding(Expression variable, Expression binding, RewritingProcess process) {
			if ( ! variable.equals(binding)) {
				equalitiesMap.put(variable, binding);
			}
		}

		private void setBinding(Map<Expression, Expression> equalitiesMap, Expression variable, Expression binding) {
			if ( ! variable.equals(binding)) {
				equalitiesMap.put(variable, binding);
			}
		}

		private boolean indexIsBound(Expression index) {
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
		Expression getRepresentative(Expression term, RewritingProcess process) {
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
		Expression getRepresentative(Expression term, boolean recordDirectBindingToRepresentative, RewritingProcess process) {
			Expression current = term;
			Expression currentBinding;
			while (termTheory.isVariableTerm(current, process) && (currentBinding = getBinding(current)) != null) {
				current = currentBinding;
			}
			// now, 'current' is in the chain started at term,
			// and it is either a constant or a variable without binding, therefore it is the equivalence class representative.
			if (recordDirectBindingToRepresentative && termTheory.isVariableTerm(term, process)) {
				setBinding(term, current, process); // optional recording so that we do not need to traverse the entire chain next time
			}
			return current;
		}
		
		private class Contradiction extends Error {};

		@Override
		public Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
			Constraint result;

			Expression simplifiedSplitterGivenConstraint = simplifySplitterGivenConstraint(splitter, process);

			if (simplifiedSplitterGivenConstraint.equals(splitterSign)) {
				result = this; // splitter is redundant given constraint
			}
			else if (simplifiedSplitterGivenConstraint.equals( ! splitterSign)) {
				result = null; // splitter is contradictory given constraint
			}
			else {
				try {
					if (splitterSign) {
						result = applyNormalizedSplitter(simplifiedSplitterGivenConstraint, process);
					}
					else {
						result = applyNormalizedSplitterNegation(simplifiedSplitterGivenConstraint, process);
					}
				}
				catch (Contradiction e) {
					result = null;
				}
			}

			return result;
		}

		private Constraint applyNormalizedSplitter(Expression splitter, RewritingProcess process) {
			Constraint newConstraint = new Constraint(this);
			newConstraint.applySplitterDestructively(splitter, process);
			return newConstraint;
		}

		private Constraint applyNormalizedSplitterNegation(Expression splitter, RewritingProcess process) {
			Constraint newConstraint = new Constraint(this);
			newConstraint.applySplitterNegationDestructively(splitter, process);
			return newConstraint;
		}

		@Override
		public Expression modelCount(RewritingProcess process) {
			
			List<Expression> numberOfPossibleValuesForIndicesSoFar = new LinkedList<Expression>();
			
			for (Expression index : indices) {
				if ( ! indexIsBound(index)) {
					Expression numberOfPossibleValuesForIndex = computerNumberOfPossibleValuesFor(index, process);
					numberOfPossibleValuesForIndicesSoFar.add(numberOfPossibleValuesForIndex);
				}
			}
			
			Expression result = Times.make(numberOfPossibleValuesForIndicesSoFar);
			result = timesRewriter.rewrite(result, process);
			result = makeModelCountConditionedOnUndeterminedSplitters(
					result,
					getSplittersToBeSatisfied(process), getSplittersToBeNotSatisfied(process),
					EqualityOnTermsTheory.this,
					process);
			
			return result;
		}

		/**
		 * @param index
		 * @param process
		 * @return
		 */
		private Expression computerNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
			long numberOfNonAvailableValues = getDisequals(index).size();
			long typeSize = GrinderUtil.getTypeCardinality(index, process);
			Expression numberOfPossibleValuesForIndex;
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
			return numberOfPossibleValuesForIndex;
		}

		private Collection<Expression> getSplittersToBeSatisfied(RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Expression freeVariable : equalitiesMap.keySet()) {
				if ( ! indices.contains(freeVariable)) {
					Expression representative = getRepresentative(freeVariable, process);
					// Note that a free variable's representative is never an index, because
					// in splitters indices always come before free variables,
					// and that is the order of the binding.
					// A splitter with a free variable as the first term will always have another free variable
					// or a constant on the right-hand side.
					// This matters because the conditional model count has to be in terms of
					// free variables and constants only, never indices.
					if ( ! representative.equals(freeVariable)) {
						Expression splitter = Expressions.apply(FunctorConstants.EQUALITY, freeVariable, representative); // making it with apply instead of Disequality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
			return result;
		}

		private Collection<Expression> getSplittersToBeNotSatisfied(RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Map.Entry<Expression, NonEqualityConstraints> entry : nonEqualityConstraintsMap.entrySet()) {
				assert termTheory.isVariableTerm(entry.getKey(), process);
				Expression variable = entry.getKey();
				if ( ! indices.contains(variable)) { // if variable is free
					for (Expression disequal : ((DisequalitiesConstraints) entry.getValue()).getDisequals()) {
						Expression splitter = Expressions.apply(FunctorConstants.EQUALITY, variable, disequal); // making it with apply instead of Disequality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
						result.add(splitter);
					}
				}
			}
			return result;
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
//			Collection<Expression> result = getOrUseDefault(nonEqualityConstraintsMap, variable, emptyList());
			return result;
		}

		private boolean termsAreExplicitlyConstrainedToBeEqual(Expression variable, Expression otherTerm, RewritingProcess process) {
			boolean result = getRepresentative(variable, process).equals(getRepresentative(otherTerm, process));
			return result;
		}

		private boolean termsAreExplicitlyConstrainedToBeDisequal(Expression term1, Expression term2, RewritingProcess process) {
			Expression representative1 = getRepresentative(term1, process);
			Expression representative2 = getRepresentative(term2, process);
			boolean result = representativesAreExplicitlyConstrainedToBeDisequal(representative1, representative2, process);
			return result;
		}

		private boolean representativesAreExplicitlyConstrainedToBeDisequal(Expression representative1, Expression representative2, RewritingProcess process) {
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
		public Expression checkIfSplitterOrItsNegationIsImplied(Expression splitter, RewritingProcess process) {
			if (termsAreExplicitlyConstrainedToBeEqual(splitter.get(0), splitter.get(1), process)) {
				// algorithm keeps all implied equalities represented, so we can conclude 'true' here.
				return TRUE;
			}
			
			if (algorithmKeepsAllDisequalitiesRepresented()) {
				if (termsAreExplicitlyConstrainedToBeDisequal(splitter.get(0), splitter.get(1), process)) {
					return FALSE;
				}
			}
			else {
				Constraint underSplitter = this.applySplitter(true, splitter, process);
				if (underSplitter == null) {
					return FALSE; // if equality is incompatible with constraint, the constraint implies the disequality
					// TODO: can we keep and use the computation performed here?
				}
			}
			
			return splitter;
		}
		
		private boolean algorithmKeepsAllDisequalitiesRepresented() {
			// the current algorithm does not try to propagate the consequences of disequalities,
			// so if the particular term equalityTheory disequalities implies further facts,
			// the algorithm does not represent all disequalities at all times.
			boolean result = ! termTheory.disequalityBetweenTermsImpliesFurtherFacts();
			return result;
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
		public String toString() {
			String result = "Equalities: " + equalitiesMap + ", disequalities: " + super.toString();
			return result; 
		}
	}
}