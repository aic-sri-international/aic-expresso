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

import static com.sri.ai.util.Util.getOrUseDefault;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.util.Util;

@SuppressWarnings("serial")
/**
 * Represents and manipulates constraints in the theory of equalities of symbols (variables and constants).
 */
@Beta
abstract public class AbstractSymbolEqualityConstraint extends LinkedHashMap<Expression, Collection<Expression>> implements TheoryConstraint {

	abstract public AbstractSymbolEqualityConstraint make();
	
	abstract public AbstractSymbolEqualityConstraint make(AbstractSymbolEqualityConstraint another);
	
	@Override
	abstract public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process);

	@Override
	abstract public Expression solution(Collection<Expression> indices, RewritingProcess process);

	public AbstractSymbolEqualityConstraint() {
		super();
	}

	public AbstractSymbolEqualityConstraint(AbstractSymbolEqualityConstraint another) {
		super(another);
	}

	/**
	 * Constructs the theory constraint given a conjunction of disequalities (or a single disequality),
	 * each of them involving at least one variable (and therefore being satisfiable by themselves),
	 * a collection of indices, and a rewriting process.
	 * @param disequalitiesConjunction a conjunction of disequalities, or a single disequality
	 * @param indices a collection of indices
	 * @param process a rewriting process
	 */
	public AbstractSymbolEqualityConstraint(Expression disequalitiesConjunction, Collection<Expression> indices, RewritingProcess process) {
		if (disequalitiesConjunction.equals(Expressions.FALSE)) {
			throw new Error("Cannot create a " + getClass() + " from a false constraint");
		}

		if ( ! disequalitiesConjunction.equals(Expressions.TRUE)) {

			assert disequalitiesConjunction.hasFunctor(FunctorConstants.DISEQUALITY) || disequalitiesConjunction.hasFunctor(FunctorConstants.AND);
			
			List<Expression> conjuncts = And.getConjuncts(disequalitiesConjunction);

			for (Expression disequalityConjunct : conjuncts) {

				assert disequalityConjunct.hasFunctor(FunctorConstants.DISEQUALITY);
				assert disequalityConjunct.numberOfArguments() == 2;
				
				Expression term1 = disequalityConjunct.get(0);
				Expression term2 = disequalityConjunct.get(1);

				assert process.isVariable(term1) || process.isVariable(term2);

				registerDisequality(term1, term2, indices, process);
			}
		}
	}

	protected void registerDisequality(Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(term1, term2, indices, process)) {
			Util.addToCollectionValuePossiblyCreatingIt(this, term1, term2, LinkedHashSet.class);
		}
		else {
			Util.addToCollectionValuePossiblyCreatingIt(this, term2, term1, LinkedHashSet.class);
		}
		// TODO: we could actually use a simpler ordering here (that does not distinguish between indices and free variables) for the satisfiability case
		// but we are re-using code for both model counting and satisfiability.
		// May be an optimization option in the future.
	}

	@Override
	public TheoryConstraint applySplitter(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);

		if (equalityIsInconsistentWithConstraintMap(variable, otherTerm, this)) {
			return null;
		}
		
		TheoryConstraint newConstraint = make();
		addAllDisequalitiesFromVariableToDisequalitiesToOtherTermInNewConstraint(variable, otherTerm, newConstraint, this, indices, process);
		copyEntriesForAllKeysThatAreNotVariableOrOtherTermWhileReplacingVariableByOtherTermIfNeeded(newConstraint, variable, otherTerm, indices, process);
		
		return newConstraint;
	}

	@Override
	public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);

		TheoryConstraint newConstraint = make(this);
		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(variable, otherTerm, indices, process)) {
			copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(newConstraint, variable, otherTerm, this);
		}
		else {
			copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(newConstraint, otherTerm, variable, this);
		}
		return newConstraint;
	}

	private static boolean equalityIsInconsistentWithConstraintMap(Expression variable, Expression otherTerm, TheoryConstraint constraint) {
		boolean result =
				getDistinctPredefinedTermsFrom(variable, constraint).contains(otherTerm) ||
				getDistinctPredefinedTermsFrom(otherTerm, constraint).contains(variable);
		return result;
	}

	private static Collection<Expression> getDistinctPredefinedTermsFrom(Expression variable, TheoryConstraint constraint) {
		Collection<Expression> result = getOrUseDefault(((AbstractSymbolEqualityConstraint) constraint), variable, emptyList());
		return result;
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
					((AbstractSymbolEqualityConstraint)newConstraint).put(entry.getKey(), newDistinctFromKey); // puts same disequalities in new constraints, but this incorrectly includes 'variable'
					newDistinctFromKey.remove(variable); // so we remove it from the set (it is already in 'newConstraint')
					addDisequalityToConstraintDestructively(entry.getKey(), otherTerm, newConstraint, indices, process); // add 'otherTerm' wherever appropriate.
				}
				else { // for those not constrained to be different from variable, we simply re-use the set of constraints in new constraint map
					((AbstractSymbolEqualityConstraint)newConstraint).put(entry.getKey(), entry.getValue()); // shares sets between constraint maps
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
		Set<Expression> distinctFromTerm1 = (Set<Expression>) Util.getValuePossiblyCreatingIt(((AbstractSymbolEqualityConstraint)constraint), term1, LinkedHashSet.class);
		distinctFromTerm1.add(term2);
	}

	private static void copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2(
			TheoryConstraint newConstraint, Expression term1, Expression term2, TheoryConstraint oldConstraintMap) {
		
		Set<Expression> distinctTermsFromTerm1 = new LinkedHashSet<Expression>(getDistinctPredefinedTermsFrom(term1, oldConstraintMap));
		distinctTermsFromTerm1.add(term2);
		((AbstractSymbolEqualityConstraint)newConstraint).put(term1, distinctTermsFromTerm1);
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