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
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable.TypeSizeOfLogicalVariable;
import com.sri.ai.util.Util;

@SuppressWarnings("serial")
/**
 * Represents and manipulates constraints in the theory of equalities of symbols (variables and constants) for model counting purposes.
 * 
 * It does this by keeping the equivalent of a conjunction of disequalities in a map from each variable to
 * the terms constraint to be distinct from it that are defined before it in the choosing order
 * used for counting the number of solutions.
 * The ordering chosen is the String order for the String representation of the terms.
 * This is useful because we use the Counting Principle for each variable in the total ordering chosen.
 * For each variable, we have N - D options, where N is its type size and D is the number of values it must be distinct from.
 * These values are the constants it is constrained to be distinct from, and variables that have their <i>already chosen</i>
 * (coming in the ordering first), assuming that their values do not overlap.
 * This counting will only be correct, however, if there is a guarantee that these predefined distinct terms are constrained to be distinct from each
 * other (a condition similar but not identical to what is called "normalized constraints" in the lifted inference literature).
 * The algorithm does check and enforces this guarantee.
 */
@Beta
public class SymbolEqualityModelCountingConstraint extends LinkedHashMap<Expression, Collection<Expression>> implements TheoryConstraint {

	public SymbolEqualityModelCountingConstraint() {
		super();
	}

	public SymbolEqualityModelCountingConstraint(SymbolEqualityModelCountingConstraint another) {
		super(another);
	}

	public SymbolEqualityModelCountingConstraint(Expression atomsConjunction, Collection<Expression> indices, RewritingProcess process) {
		if (atomsConjunction.equals(Expressions.FALSE)) {
			throw new Error("Cannot create a SymbolEqualityModelCountingConstraint from a false constraint");
		}

		if ( ! atomsConjunction.equals(Expressions.TRUE)) {

			List<Expression> conjuncts = And.getConjuncts(atomsConjunction);

			for (Expression disequalityConjunct : conjuncts) {
				Expression term1 = disequalityConjunct.get(0);
				Expression term2 = disequalityConjunct.get(1);
				if (process.isVariable(term1) && whenChoosingValueForVariableOtherTermIsAlreadyDefined(term1, term2, indices, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(this, term1, term2, LinkedHashSet.class);
				}
				if (process.isVariable(term2) && whenChoosingValueForVariableOtherTermIsAlreadyDefined(term2, term1, indices, process)) {
					Util.addToCollectionValuePossiblyCreatingIt(this, term2, term1, LinkedHashSet.class);
				}
			}
		}
	}

	@Override
	public Expression pickAtom(Collection<Expression> indices, RewritingProcess process) {

		for (Expression index : indices) {
			
			Collection<Expression> distinctPredefinedTermsForVariable1 = getDistinctPredefinedTermsFrom(index, this);
			
			for (Expression distinctPredefinedVariable2 : distinctPredefinedTermsForVariable1) {
				if (process.isVariable(distinctPredefinedVariable2)) {
			
					Collection<Expression> distinctPredefinedTermsForVariable2 =
							getDistinctPredefinedTermsFrom(distinctPredefinedVariable2, this);
					Expression distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 =
							getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
									distinctPredefinedTermsForVariable1, distinctPredefinedTermsForVariable2, distinctPredefinedVariable2, this);
					if (distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 != null) {
						
						Expression atom =
								PlainCardinalityDPLLWithFreeVariables
								.makeSplitterWithIndexIfAnyComingFirst(
										distinctPredefinedVariable2,
										distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2, indices);

						return atom;
					}
				}
			}
		}
		
		return null;
	}

	@Override
	public Expression solution(Collection<Expression> indices, RewritingProcess process) {
		
		long resultValue = 1;
		
		for (Expression index : indices) {
			long typeSize = getTypeSize(index, process);
			Collection<Expression> setOfDistinctTerms = get(index);
			long numberOfNonAvailableValues = setOfDistinctTerms == null? 0 : (long) setOfDistinctTerms.size();
			resultValue *= typeSize - numberOfNonAvailableValues;
		}
		
		return Expressions.makeSymbol(resultValue);
	}

	@Override
	public TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);

		if (equalityIsInconsistentWithConstraintMap(variable, otherTerm, this)) {
			return null;
		}
		
		TheoryConstraint newConstraint = new SymbolEqualityModelCountingConstraint();
		addAllDisequalitiesFromVariableToDisequalitiesToOtherTermInNewConstraint(variable, otherTerm, newConstraint, this, indices, process);
		copyEntriesForAllKeysThatAreNotVariableOrOtherTermWhileReplacingVariableByOtherTermIfNeeded(newConstraint, variable, otherTerm, indices, process);
		
		return newConstraint;
	}

	@Override
	public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);

		TheoryConstraint newConstraint = new SymbolEqualityModelCountingConstraint(this);
		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(variable, otherTerm, indices, process)) {
			copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(newConstraint, variable, otherTerm, this);
		}
		else {
			copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(newConstraint, otherTerm, variable, this);
		}
		return newConstraint;
	}

	private static long getTypeSize(Expression variable, RewritingProcess process) {
		TypeSizeOfLogicalVariable typeSizes = (TypeSizeOfLogicalVariable) process
				.getGlobalObject(CardinalityTypeOfLogicalVariable.PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE);
		long typeSize = typeSizes.size(variable, process);
		return typeSize;
	}

	private static Expression getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
			Collection<Expression> distinctPredefinedTermsForVariable1,
			Collection<Expression> distinctPredefinedTermsForVariable2,
			Expression variable2,
			TheoryConstraint constraint) {
	
		for (Expression distinctPredefinedTermForVariable1 : distinctPredefinedTermsForVariable1) {
			if ( ! distinctPredefinedTermForVariable1.equals(variable2)) {
				if (distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(distinctPredefinedTermForVariable1, variable2, distinctPredefinedTermsForVariable2, constraint)) {
					return distinctPredefinedTermForVariable1;
				}
			}
		}
		return null;
	}

	private static boolean distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(
			Expression distinctPredefinedTermForVariable1, Expression variable2,
			Collection<Expression> distinctPredefinedTermsForVariable2, TheoryConstraint constraint) {
	
		if ( ! distinctPredefinedTermsForVariable2.contains(distinctPredefinedTermForVariable1)) {
			Collection<Expression> distinctPredefinedTermsForDistinctPredefinedTermForVariable1 =
					getDistinctPredefinedTermsFrom(distinctPredefinedTermForVariable1, constraint);
			if ( ! distinctPredefinedTermsForDistinctPredefinedTermForVariable1.contains(variable2)) {
				return true;
			}
		}
		return false;
	}

	private static boolean equalityIsInconsistentWithConstraintMap(Expression variable, Expression otherTerm, TheoryConstraint constraint) {
		return getDistinctPredefinedTermsFrom(variable, constraint).contains(otherTerm) ||
				getDistinctPredefinedTermsFrom(otherTerm, constraint).contains(variable);
	}

	private static Collection<Expression> getDistinctPredefinedTermsFrom(Expression variable, TheoryConstraint constraint) {
		return Util.getOrUseDefault(((SymbolEqualityModelCountingConstraint)constraint), variable, Collections.<Expression> emptyList());
	}

	private static void addAllDisequalitiesFromVariableToDisequalitiesToOtherTermInNewConstraint(Expression variable, Expression otherTerm, TheoryConstraint newConstraint, TheoryConstraint constraint, Collection<Expression> indices, RewritingProcess process) {
		Collection<Expression> distinctOnesFromVariable = getDistinctPredefinedTermsFrom(variable, constraint);
		for (Expression distinctFromVariable : distinctOnesFromVariable) {
			addDisequalityToConstraintDestructively(otherTerm, distinctFromVariable, newConstraint, indices, process);
		}
	}

	private void copyEntriesForAllKeysThatAreNotVariableOrOtherTermWhileReplacingVariableByOtherTermIfNeeded(TheoryConstraint newConstraint, Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		for (Map.Entry<Expression, Collection<Expression>> entry : entrySet()) {
			if ( ! entry.getKey().equals(variable) && ! entry.getKey().equals(otherTerm)) {
				if (entry.getValue().contains(variable)) { // for those keys that are are constrained to be distinct from variable
					// we will create a new set of constraints, put it under the key in the new constraint map, remove variable, and add other term instead
					Set<Expression> newDistinctFromKey = new LinkedHashSet<Expression>(entry.getValue());
					((SymbolEqualityModelCountingConstraint)newConstraint).put(entry.getKey(), newDistinctFromKey); // puts same disequalities in new constraints, but this incorrectly includes 'variable'
					newDistinctFromKey.remove(variable); // so we remove it from the set (it is already in 'newConstraint')
					addDisequalityToConstraintDestructively(entry.getKey(), otherTerm, newConstraint, indices, process); // add 'otherTerm' wherever appropriate.
				}
				else { // for those not constrained to be different from variable, we simply re-use the set of constraints in new constraint map
					((SymbolEqualityModelCountingConstraint)newConstraint).put(entry.getKey(), entry.getValue()); // shares sets between constraint maps
				}
			}
		}
	}

	/** Assumes at least one of the two terms is a variable. */
	private static void addDisequalityToConstraintDestructively(
			Expression term1, Expression term2, TheoryConstraint constraint, Collection<Expression> indices, RewritingProcess process) {
		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(term1, term2, indices, process)) {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term1, term2, constraint);
		}
		else {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term2, term1, constraint);
		}
	}

	private static void addFirstTermToDistinctTermsFromSecondTermDestructively(Expression term1, Expression term2, TheoryConstraint constraint) {
		Set<Expression> distinctFromTerm1 = (Set<Expression>) Util.getValuePossiblyCreatingIt(((SymbolEqualityModelCountingConstraint)constraint), term1, LinkedHashSet.class);
		distinctFromTerm1.add(term2);
	}

	private static void copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(
			TheoryConstraint newConstraint, Expression term1, Expression term2, TheoryConstraint oldConstraintMap) {
		Set<Expression> distinctTermsFromTerm1 = new LinkedHashSet<Expression>(getDistinctPredefinedTermsFrom(term1, oldConstraintMap));
		distinctTermsFromTerm1.add(term2);
		((SymbolEqualityModelCountingConstraint)newConstraint).put(term1, distinctTermsFromTerm1);
	}

	private static boolean whenChoosingValueForVariableOtherTermIsAlreadyDefined(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isConstant(otherTerm) || variablePrecedesAnother(otherTerm, variable, indices);
		return result;
	}

	/**
	 * Indicates whether variable1 precedes variable2 in the total ordering
	 */
	private static boolean variablePrecedesAnother(Expression variable1, Expression variable2, Collection<Expression> indices) {
		boolean result;
		if (indices.contains(variable1)) { // index
			if ( ! indices.contains(variable2)) { // free variable
				result = false; // free variables always precedes indices
			}
			else { // both are indices
				result = variable2.toString().compareTo(variable1.toString()) < 0; // indices are compared alphabetically
			}
		}
		else if (indices.contains(variable2)) { // variable1 is free variable and variable2 is index
			result = true; // free variable always precedes indices
		}
		else { // neither is index
			result = variable2.toString().compareTo(variable1.toString()) < 0;	// alphabetically		
		}
		return result;
	}
}