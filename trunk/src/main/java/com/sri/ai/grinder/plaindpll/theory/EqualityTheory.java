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
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.addAllForEachEntry;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getTransformedSubMap;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoSetOrSameIfNoDistinctElementInstances;
import static com.sri.ai.util.Util.removeAll;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
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
import com.sri.ai.grinder.plaindpll.api.ConjunctiveConstraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.grinder.plaindpll.core.AbstractRuleOfProductConstraint;
import com.sri.ai.grinder.plaindpll.core.AbstractTheory;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SimplifyLiteralGivenDisequality;
import com.sri.ai.grinder.plaindpll.problemtype.Satisfiability;
import com.sri.ai.grinder.plaindpll.problemtype.Tautologicality;
import com.sri.ai.grinder.plaindpll.theory.term.FunctionalTermTheory;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
@Beta
/** 
 * A {@link Theory} for equality literals.
 */
public class EqualityTheory extends AbstractTheory {
	
	// these are just named types for improving readability
	private interface NonEqualitiesForSingleTerm extends Map<String, Collection<Expression>> {}
	@SuppressWarnings("serial")
	private class LinkedHashNonEqualitiesForSingleVariable extends LinkedHashMap<String, Collection<Expression>> implements NonEqualitiesForSingleTerm {
	}
	
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

	/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
	public interface NonEqualitiesConstraintForSingleVariable extends ConjunctiveConstraint {

		/**
		 * A copy "constructor".
		 * The reason this is not a real constructor is because the signature would be easily confused with the constructor from variable 
		 * since {@link DisequalitiesConstraintForSingleVariable} implements {@link Expression}.
		 * @param another
		 * @param parentEqualityConstraint
		 * @return
		 */
		NonEqualitiesConstraintForSingleVariable copy(EqualityConstraint parentEqualityConstraint);
		
		/**
		 * Adds a constraint to this object.
		 * @param functor
		 * @param term
		 * @param process
		 */
		void addNonEqualityConstraintDestructively(String functor, Expression term, RewritingProcess process) throws Contradiction;

		/**
		 * Returns a pair of the variable and non-equality constraints map after updating representatives in their
		 * representation, or <code>null</code> if there are no changes.
		 * @param process
		 * @return
		 * @throws Contradiction
		 */
		Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(RewritingProcess process) throws Contradiction;

		/**
		 * Returns splitters on free variables required to hold for this constraint to hold.
		 * @return
		 */
		List<Expression> getSplittersToBeSatisfied();
		
		/**
		 * Returns splitters on free variables the negations of which are required to hold for this constraint to hold.
		 * @return
		 */
		List<Expression> getSplittersToBeNotSatisfied();
	}

	/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
	@SuppressWarnings("serial")
	public abstract class AbstractNonEqualitiesConstraintForSingleVariable extends AbstractExpressionWrapper implements NonEqualitiesConstraintForSingleVariable {
		protected Expression variable;
		protected EqualityConstraint parentEqualityConstraint;
		protected long cachedIndexDomainSize = -1;

		public AbstractNonEqualitiesConstraintForSingleVariable(Expression variable, EqualityConstraint parentEqualityConstraint) {
			this.variable = variable;
			this.cachedIndexDomainSize = -1;
			this.parentEqualityConstraint = parentEqualityConstraint;
		}
		
		public AbstractNonEqualitiesConstraintForSingleVariable clone() {
			assert false : "Cloning of " + getClass() + " not implemented yet.";
			return null;
		}

		@Override
		public Theory getTheory() {
			return EqualityTheory.this;
		}

		@Override
		public Collection<Expression> getSupportedIndices() {
			return list(variable);
		}

		protected long getIndexDomainSize(RewritingProcess process) {
			if (cachedIndexDomainSize == -1) {
				cachedIndexDomainSize = getTypeCardinality(variable, process);
			}
			return cachedIndexDomainSize;
		}

		@Override
		public ConjunctiveConstraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
			assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
			return null;
		}

		@Override
		public Expression normalize(Expression expression, RewritingProcess process) {
			assert false : (new Object(){}).getClass().getEnclosingMethod() + " not implemented yet."; // more robust to method renaming
			return null;
		}
	}

	/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
	@SuppressWarnings("serial")
	public class DisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {
		private boolean ownMyDisequals;
		private Collection<Expression> disequals;
		private Collection<Expression> uniquelyValuedDisequals; // disequals constrained to be disequal from all uniquely-valued disequals added before themselves. If this set reaches variable's domain size, there will be no value left for it and an inconsistency is indicated.

		public DisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraint parentEqualityConstraint) {
			super(variable, parentEqualityConstraint);
			this.ownMyDisequals = true;
			this.disequals = Util.set();
			this.uniquelyValuedDisequals = Util.set();
		}

		@Override
		public DisequalitiesConstraintForSingleVariable copy(EqualityConstraint parentEqualityConstraint) {
			DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, parentEqualityConstraint);
			result.cachedIndexDomainSize = cachedIndexDomainSize;
			result.ownMyDisequals = false;
			result.disequals = disequals;
			result.uniquelyValuedDisequals = uniquelyValuedDisequals;
			return result;
		}
		
		public void addNonEqualityConstraintDestructively(String functor, Expression term, RewritingProcess process) throws Contradiction {
			if ( ! ownMyDisequals) {
				disequals = new LinkedHashSet<Expression>(disequals);
				if (getIndexDomainSize(process) != -1) {
					uniquelyValuedDisequals = new LinkedHashSet<Expression>(uniquelyValuedDisequals);
				}
				ownMyDisequals = true;
			}
			disequals.add(term);
			updateUniqueValuedDisequals(term, process);
			resetInnerExpression();
		}

		private void updateUniqueValuedDisequals(Expression term, RewritingProcess process) throws Contradiction {
			if (getIndexDomainSize(process) != -1) {
				if (forAll(uniquelyValuedDisequals, u -> areConstrainedToBeDisequal(u, term, process))) {
					uniquelyValuedDisequals.add(term);
				}
				if (uniquelyValuedDisequals.size() >= getIndexDomainSize(process)) {
					throw new Contradiction();
				}
			}
		}
		
		@Override
		public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
			if ( ! indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, it must be a set with this.index as only element.
				for (Expression disequal : disequals) {
					if (isVariableTerm(disequal, process)) { // we can restrict y to variables because at least one of y or t must be a variable (otherwise they would be two constants and we already know those are disequal).
						Expression splitter  = getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(disequal, process);
						if (splitter != null) {
							return splitter;
						}
					}
				}
			}
			
			return null;
		}

		private Expression getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(Expression disequal, RewritingProcess process) {
			for (Expression anotherDisequal : disequals) {
				if ( ! anotherDisequal.equals(disequal)) {
					Expression splitter = termTheory.getSplitterTowardDisunifyingDistinctTerms(disequal, anotherDisequal, process); // if function applications, we need to disunify arguments first, for instance.
					if (splitter != null) {
						return splitter; // need to disunify first
					}
					else if ( ! areConstrainedToBeDisequal(disequal, anotherDisequal, process)) { // already disunified
						splitter = makeSplitterFromFunctorAndTwoTerms(EQUALITY, disequal, anotherDisequal, parentEqualityConstraint.getSupportedIndices(), process);
						return splitter;
					}
				}
			}
			return null;
		}

		private boolean areConstrainedToBeDisequal(Expression disequal, Expression anotherDisequal, RewritingProcess process) {
			boolean result = parentEqualityConstraint.termsAreExplicitlyConstrainedToBeDisequal(disequal, anotherDisequal, process);
			return result;
		}

		@Override
		public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Expression numberOfPossibleValuesForIndex;
			long numberOfNonAvailableValues = disequals.size();
			long typeSize = getIndexDomainSize(process);
			if (typeSize == -1) {
				Expression indexType = process.getContextualSymbolType(variable);
				if (indexType == null) {
					indexType = new DefaultSyntacticFunctionApplication(TYPE, variable);
				}
				Expression indexTypeCardinality = apply(CARDINALITY, indexType);
				numberOfPossibleValuesForIndex = Minus.make(indexTypeCardinality, makeSymbol(numberOfNonAvailableValues));
			}
			else {
				numberOfPossibleValuesForIndex = makeSymbol(max(0, typeSize - numberOfNonAvailableValues));
			}
			return numberOfPossibleValuesForIndex;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			Expression result;
			
			if (splitter.get(0).equals(variable) && disequals.contains(splitter.get(1))) {
				result = FALSE;
			}
			else if (splitter.get(1).equals(variable) && disequals.contains(splitter.get(0))) {
				result = FALSE;
			}
			else {
				result = splitter;
			}
			
			return result;
		}

		/**
		 * Returns a pair of the variable and non-equality constraints map after updating representatives in their
		 * representation, or <code>null</code> if there are no changes.
		 * @param process
		 * @return
		 * @throws Contradiction
		 */
		public Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(RewritingProcess process) throws Contradiction {
			Pair<Expression, NonEqualitiesForSingleTerm> result = new Pair<Expression, NonEqualitiesForSingleTerm>();
			result.first = variable.replaceAllOccurrences(t -> parentEqualityConstraint.getRepresentative(t, process), process);
			
			Function<Expression, Expression> getRepresentative = t -> parentEqualityConstraint.getRepresentative(t, process);
			Function<Expression, Expression> replaceAllTermsByRepresentatives = e -> e.replaceAllOccurrences(getRepresentative, process);
			Collection<Expression> newDisequals = mapIntoSetOrSameIfNoDistinctElementInstances(disequals, replaceAllTermsByRepresentatives);
			if (result.first == variable && newDisequals == disequals) {
				result = null;
			}
			else {
				if (newDisequals.contains(result.first)) {
					throw new Contradiction();
				}
				result.second = new LinkedHashNonEqualitiesForSingleVariable();
				result.second.put(DISEQUALITY, newDisequals);
			}
				
			return result;
		}

		public List<Expression> getSplittersToBeSatisfied() {
			return emptyList();
		}

		public List<Expression> getSplittersToBeNotSatisfied() {
			List<Expression> result = new LinkedList<Expression>();
			for (Expression disequal : disequals) {
				Expression splitter = apply(EQUALITY, variable, disequal); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
				result.add(splitter);
			}
			return result;
		}

		@Override
		protected Expression computeInnerExpression() {
			List<Expression> arguments = mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d));
			Expression result = And.make(arguments);
			return result;
		}
	}
	
	/**
	 * Represents and manipulates constraints in the theoryWithEquality of disequalities of terms (variables and constants).
	 */
	@Beta
	public class EqualityConstraint extends AbstractRuleOfProductConstraint {

		// The algorithm is based on the counting principle: to determine the model count, we
		// go over indices, in a certain order, and analyse how many possible values each one them has,
		// based on how many constants, free variables, and previous indices are constrained to be disequal from it.
		// (free variables and constants are considered less than indices in the choosing order).

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
		
		private static final long serialVersionUID = 1L;

		public Map<Expression, Expression> equalitiesMap;
		public LinkedHashMap<Expression,NonEqualitiesConstraintForSingleVariable> nonEqualitiesMap;

		public EqualityConstraint(Collection<Expression> indices) {
			super(indices);
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>();
			this.nonEqualitiesMap = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>(); 
		}

		private EqualityConstraint(EqualityConstraint another) {
			super(another.getSupportedIndices());
			this.equalitiesMap = new LinkedHashMap<Expression, Expression>(another.equalitiesMap);
			this.nonEqualitiesMap = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>(); 
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : another.nonEqualitiesMap.entrySet()) {
				nonEqualitiesMap.put(entry.getKey(), entry.getValue().copy(this)); // must copy sets to avoid interference. OPTIMIZATION: use a copy-as-needed implementation of set later.
			}
		}

		@Override
		public EqualityConstraint clone() {
			return new EqualityConstraint(this);
		}

		@Override
		public Theory getTheory() {
			return EqualityTheory.this;
		}

		@Override
		protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression index, RewritingProcess process) {
			Expression result;
			if (indexIsBound(index)) {
				result = null;
			}
			else {
				result = nonEqualitiesConstraintFor(index).pickSplitter(list(index), process);
			}
			return result;
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
							representativesEquality = makeSplitterFromFunctorAndTwoTerms(EQUALITY, oldRepresentative, newRepresentative, supportedIndices, process);
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
			
			// Go over all entries of disequality map, and if entry requires updating,
			// add it to a map from each term to NonEqualitiesForSingleTerm,
			// keeping also track of which variables got updated.
			// If multiple variables are updated to same term, take the union of their NonEqualitiesForSingleTerm.
			Function<Entry<Expression, NonEqualitiesConstraintForSingleVariable>, Pair<Expression, NonEqualitiesForSingleTerm>>
			getUpdatedTermAndNonEqualities = entry -> entry.getValue().updatedTermAndNonEqualitiesPair(process);
			
			BinaryFunction<NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm>
			unionOfNonEqualitiesIfNeeded = (previous, more) -> addAllForEachEntry(previous, more);
			
			Pair<Map<Expression, NonEqualitiesForSingleTerm>, Set<Expression>>
			updatedNonEqualitiesByTermAndDeletedVariables
			= getTransformedSubMap(nonEqualitiesMap, getUpdatedTermAndNonEqualities, unionOfNonEqualitiesIfNeeded);

			// Notes:
			// One might wonder why we did not represent the non-equal elements after the update
			// in a DisequalitiesConstraintForSingleVariable object, and then just add this new
			// object to the disequalities map.
			// There are two reasons for this:
			// - DisequalitiesConstraintForSingleVariable requires the elements non-equal to its variable
			// to contain only terms that come *before* the variable in the term ordering;
			// updating representatives may result in some non-equal terms to come *after* the
			// (also now possibly updated) variable;
			// therefore, these non-equal elements will have to be re-constrained to be non-equal from
			// the updated variable one by one anyway, and keeping them in a DisequalitiesConstraintForSingleVariable
			// would not be useful as they will probably go separate anyway;
			// - the variable may be updated to a constant, and DisequalitiesConstraintForSingleVariable
			// is only defined for variables.
			
			// now we update the disequalities map if needed:

			Map<Expression, NonEqualitiesForSingleTerm> updatedNonEqualitiesByTerm = updatedNonEqualitiesByTermAndDeletedVariables.first;
			Set<Expression> deletedVariables = updatedNonEqualitiesByTermAndDeletedVariables.second;

			// we start by removing the modified entries
			removeAll(nonEqualitiesMap, deletedVariables);

			// and we add the new disequalities. Note we cannot just put them in the map as they are, because of choosing order
			for (Map.Entry<Expression, NonEqualitiesForSingleTerm> updatedEntry : updatedNonEqualitiesByTerm.entrySet()) {
				for (Expression disequal : updatedEntry.getValue().get(DISEQUALITY)) {
					applyRepresentativesDisequalityDestructively(updatedEntry.getKey(), disequal, process);
				}
			}
		}

		/** Assumes disequality does not turn constraint into contradiction */
		private void applyRepresentativesDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
			if (termTheory.isVariableTerm(term1, process) || termTheory.isVariableTerm(term2, process)) {
				if (termTheory.isVariableTerm(term1, process) && variableIsChosenAfterOtherTerm(term1, term2, supportedIndices, process)) {
					addFirstTermAsDisequalOfSecondTerm(term1, term2, process);
				}
				else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
					addFirstTermAsDisequalOfSecondTerm(term2, term1, process);
				}
			}
			// else they are both constants, and distinct ones, so no need to do anything.
		}

		private void addFirstTermAsDisequalOfSecondTerm(Expression term1, Expression term2, RewritingProcess process) {
			NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForTerm1 = nonEqualitiesConstraintFor(term1);
			disequalitiesConstraintForTerm1.addNonEqualityConstraintDestructively(DISEQUALITY, term2, process);
		}

		private NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintFor(Expression term) {
			NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraint =
					Util.getValuePossiblyCreatingIt(nonEqualitiesMap, term, key -> makeNonEqualitiesConstraintForVariable(key));
			return nonEqualitiesConstraint;
		}

		protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable) {
			DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, this);
			return result;
		}

		@Override
		protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Expression variable : equalitiesMap.keySet()) {
				if ( ! indicesSubSet.contains(variable)) {
					Expression representative = getRepresentative(variable, process);
					// Note that a free variable's representative is never an variable, because
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
			// TODO: when nonEqualitiesMap gets consolidated into a single Constraint object, make sure it has a method getSplittersToBeSatisfied
			// that does not iterate over all variables for disequalities, since we know in advance they do not provide splitters of this sort.
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : nonEqualitiesMap.entrySet()) {
				assert termTheory.isVariableTerm(entry.getKey(), process);
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeSatisfied();
					result.addAll(subResult);
				}
			}
			return result;
		}

		@Override
		protected Collection<Expression> getSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
			Collection<Expression> result = new LinkedHashSet<Expression>();
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : nonEqualitiesMap.entrySet()) {
				assert termTheory.isVariableTerm(entry.getKey(), process);
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeNotSatisfied();
					result.addAll(subResult);
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
				numberOfPossibleValuesForIndex = nonEqualitiesConstraintFor(index).modelCount(list(index), process);
			}
			return numberOfPossibleValuesForIndex;
		}

		private boolean termsAreExplicitlyConstrainedToBeDisequal(Expression term1, Expression term2, RewritingProcess process) {
			Expression representative1 = getRepresentative(term1, process);
			Expression representative2 = getRepresentative(term2, process);
			boolean result = representativesAreExplicitlyConstrainedToBeDisequal(representative1, representative2, process);
			return result;
		}

		protected boolean representativesAreExplicitlyConstrainedToBeDisequal(Expression representative1, Expression representative2, RewritingProcess process) {
			boolean result = false;
			boolean representative1IsUniquelyNamedConstant = process.isUniquelyNamedConstant(representative1);
			boolean representative2IsUniquelyNamedConstant = process.isUniquelyNamedConstant(representative2);
			
			Expression splitter = apply(EQUALITY, representative1, representative2);
			
			if (representative1IsUniquelyNamedConstant && representative2IsUniquelyNamedConstant) {
				result = ! representative1.equals(representative2);
			}
			else if ( ! representative1IsUniquelyNamedConstant &&
					getNonEqualityConstraintOn(representative1, process).normalizeSplitterGivenConstraint(splitter, process)
					!= splitter) {
				result = true;
			}
			else if ( ! representative2IsUniquelyNamedConstant &&
					getNonEqualityConstraintOn(representative2, process).normalizeSplitterGivenConstraint(splitter, process)
					!= splitter) {
				result = true;
			}
			
			// this method looks weird right now because we are in the process of generalizing it from DisequalitiesConstraintForSingleVariable to NonEqualitiesConstraint.
			// Eventually this whole method will be a normalizeSplitterGivenConstraint for a Constraint implementation that gathers NonEqualitiesConstraints for all variables.
			
			return result;
		}

		private NonEqualitiesConstraintForSingleVariable getNonEqualityConstraintOn(Expression variable, RewritingProcess process) {
			NonEqualitiesConstraintForSingleVariable result;
			assert termTheory.isVariableTerm(variable, process) : "getDisequalitiesConstraint must be invoked for a variable but was invoked on " + variable;
			result = nonEqualitiesConstraintFor(variable);
			return result;
		}

		@Override
		protected Expression computeInnerExpression() {
			List<Expression> conjuncts = new LinkedList<Expression>();
			for (Map.Entry<Expression, Expression> entry : equalitiesMap.entrySet()) {
				conjuncts.add(Equality.make(entry.getKey(), entry.getValue()));
			}
			conjuncts.addAll(nonEqualitiesMap.values());
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
		
		public void f() {
			Expression trueSymbol = parse("true");
			System.out.println("true.getSyntaxTree().getRoot(): " + trueSymbol.getSyntaxTree().getRootTree());	
			DisequalitiesConstraintForSingleVariable d = new DisequalitiesConstraintForSingleVariable(parse("X"), this);
			System.out.println("d.getSyntaxTree().getRoot(): " + d.getSyntaxTree().getRootTree());	
			
			DefaultRewritingProcess p = new DefaultRewritingProcess(null);
			Expression th = p.getContextualSymbolType(trueSymbol);
			System.out.println("type of true symbol: " + th);
			System.out.println("type of d: " + p.getContextualSymbolType(d));
		}
	}
	
	public static void main(String[] args) {
		EqualityTheory t = new EqualityTheory(new FunctionalTermTheory());
		EqualityConstraint e = t.makeConstraint(list(parse("X")));
		e.f();
	}
}