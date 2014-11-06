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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.getOrUseDefault;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;

@SuppressWarnings("serial")
/**
 * Represents and manipulates constraints in the theory of equalities of symbols (variables and constants).
 */
@Beta
public class SymbolEqualityConstraint extends LinkedHashMap<Expression, Collection<Expression>> implements TheoryConstraint {

	public SymbolEqualityConstraint() {
		super();
	}

	public SymbolEqualityConstraint(SymbolEqualityConstraint another) {
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
	public SymbolEqualityConstraint(Expression disequalitiesConjunction, Collection<Expression> indices, RewritingProcess process) {
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

		if (equalityIsInconsistentWithConstraintMap(variable, otherTerm)) {
			return null;
		}
		
		SymbolEqualityConstraint newConstraint = new SymbolEqualityConstraint();
		addAllDisequalitiesFromVariableToDisequalitiesToOtherTermInNewConstraint(variable, otherTerm, newConstraint, this, indices, process);
		copyEntriesForAllKeysThatAreNotVariableOrOtherTermWhileReplacingVariableByOtherTermIfNeeded(newConstraint, variable, otherTerm, indices, process);
		
		return newConstraint;
	}

	@Override
	public TheoryConstraint applySplitterNegation(Expression splitter, Collection<Expression> indices, RewritingProcess process) {
		
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);

		SymbolEqualityConstraint newConstraint = new SymbolEqualityConstraint(this);
		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(variable, otherTerm, indices, process)) {
			newConstraint.copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2FromAnotherConstraint(variable, otherTerm, this);
		}
		else {
			newConstraint.copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2FromAnotherConstraint(otherTerm, variable, this);
		}
		return newConstraint;
	}

	private boolean equalityIsInconsistentWithConstraintMap(Expression variable, Expression otherTerm) {
		boolean result =
				getDistinctPredefinedTermsFrom(variable).contains(otherTerm) ||
				getDistinctPredefinedTermsFrom(otherTerm).contains(variable);
		return result;
	}

	private void addAllDisequalitiesFromVariableToDisequalitiesToOtherTermInNewConstraint(Expression variable, Expression otherTerm, SymbolEqualityConstraint newConstraint, SymbolEqualityConstraint constraint, Collection<Expression> indices, RewritingProcess process) {
		Collection<Expression> distinctOnesFromVariable = getDistinctPredefinedTermsFrom(variable);
		for (Expression distinctFromVariable : distinctOnesFromVariable) {
			newConstraint.addDisequalityToConstraintDestructively(otherTerm, distinctFromVariable, indices, process);
		}
	}

	private void copyEntriesForAllKeysThatAreNotVariableOrOtherTermWhileReplacingVariableByOtherTermIfNeeded(SymbolEqualityConstraint newConstraint, Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		for (Map.Entry<Expression, Collection<Expression>> entry : entrySet()) {
			if ( ! entry.getKey().equals(variable) && ! entry.getKey().equals(otherTerm)) {
				if (entry.getValue().contains(variable)) { // for those keys that are are constrained to be distinct from variable
					// we will create a new set of constraints, put it under the key in the new constraint map, remove variable, and add other term instead
					Set<Expression> newDistinctFromKey = new LinkedHashSet<Expression>(entry.getValue());
					((SymbolEqualityConstraint)newConstraint).put(entry.getKey(), newDistinctFromKey); // puts same disequalities in new constraints, but this incorrectly includes 'variable'
					newDistinctFromKey.remove(variable); // so we remove it from the set (it is already in 'newConstraint')
					newConstraint.addDisequalityToConstraintDestructively(entry.getKey(), otherTerm, indices, process); // add 'otherTerm' wherever appropriate.
				}
				else { // for those not constrained to be different from variable, we simply re-use the set of constraints in new constraint map
					((SymbolEqualityConstraint)newConstraint).put(entry.getKey(), entry.getValue()); // shares sets between constraint maps
				}
			}
		}
	}

	/** Assumes at least one of the two terms is a variable. */
	private void addDisequalityToConstraintDestructively(
			Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {

		if (whenChoosingValueForVariableOtherTermIsAlreadyDefined(term1, term2, indices, process)) {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term1, term2);
		}
		else {
			addFirstTermToDistinctTermsFromSecondTermDestructively(term2, term1);
		}
	}

	private void addFirstTermToDistinctTermsFromSecondTermDestructively(Expression term1, Expression term2) {
		Set<Expression> distinctFromTerm1 = (Set<Expression>) Util.getValuePossiblyCreatingIt(((SymbolEqualityConstraint) this), term1, LinkedHashSet.class);
		distinctFromTerm1.add(term2);
	}

	private void copySetOfDistinctTermsForTerm1AndAddDisequalityFromTerm2FromAnotherConstraint(
			Expression term1, Expression term2, SymbolEqualityConstraint oldConstraintMap) {
		
		Set<Expression> distinctTermsFromTerm1 = new LinkedHashSet<Expression>(oldConstraintMap.getDistinctPredefinedTermsFrom(term1));
		distinctTermsFromTerm1.add(term2);
		put(term1, distinctTermsFromTerm1);
	}

	private boolean whenChoosingValueForVariableOtherTermIsAlreadyDefined(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isConstant(otherTerm) || EqualityTheory.variablePrecedesAnother(otherTerm, variable, indices);
		return result;
	}

	public Expression getMostRequiredSplitter(Expression splitterCandidate, Collection<Expression> indices, RewritingProcess process) {
		// assume splitterCandidate is of the form X = T, for X the index if either of them is
		Expression termX = splitterCandidate.get(0);
		Expression termT = splitterCandidate.get(1);
		Collection<Expression> distinctTermsFromX = getDistinctPredefinedTermsFrom(termX);
		Expression distinctTermFromXNotConstrainedToBeDistinctFromT =
				Util.getFirstSatisfyingPredicateOrNull(distinctTermsFromX, term -> ! term.equals(termT) && ! termsAreConstrainedToBeDifferent(term, termT, process));
		if (distinctTermFromXNotConstrainedToBeDistinctFromT != null) {
			splitterCandidate = EqualityTheory.makeSplitterFromTwoTerms(termT, distinctTermFromXNotConstrainedToBeDistinctFromT, indices, process);
			splitterCandidate = getMostRequiredSplitter(splitterCandidate, indices, process);
		}
		return splitterCandidate;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indices, RewritingProcess process) {

		for (Expression index : indices) {
			
			Collection<Expression> distinctPredefinedTermsForVariable1 = getDistinctPredefinedTermsFrom(index);
			
			for (Expression distinctPredefinedVariable2 : distinctPredefinedTermsForVariable1) {
				if (process.isVariable(distinctPredefinedVariable2)) {
			
					Expression distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 =
							lookForTermInCollectionThatIsNotVariableAndIsNotDistinctFromVariable(distinctPredefinedTermsForVariable1, distinctPredefinedVariable2);
					
					if (distinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctPredefinedForVariable2 != null) {
						
						Expression atom =
								EqualityTheory
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

	private Expression lookForTermInCollectionThatIsNotVariableAndIsNotDistinctFromVariable(Collection<Expression> terms, Expression variable) {
		Collection<Expression> distinctPredefinedTermsForVariable2 = getDistinctPredefinedTermsFrom(variable);
		Expression result =
				getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
						terms, distinctPredefinedTermsForVariable2, variable);
		return result;
	}

	private static Times timesRewriter = new Times();

	@Override
	public Expression modelCount(Collection<Expression> indices, RewritingProcess process) {
		
		ArrayList<Expression> indexNumbersOfPossibleValues = new ArrayList<Expression>(indices.size());
		
		for (Expression index : indices) {
			Collection<Expression> setOfDistinctTerms = get(index);
			long numberOfNonAvailableValues = setOfDistinctTerms == null? 0 : (long) setOfDistinctTerms.size();

			long typeSize = GrinderUtil.getTypeCardinality(index, process);
			Expression indexNumberOfPossibleValues;
			if (typeSize == -1) {
				Expression indexType = process.getContextualSymbolType(index);
				if (indexType == null) {
					// throw new Error("Type of " + index + " unknown but needed for symbolic cardinality computation.");
					indexType = new DefaultSyntacticFunctionApplication(FunctorConstants.TYPE, index);
				}
				Expression indexTypeCardinality = apply(CARDINALITY, indexType);
				indexNumberOfPossibleValues = Minus.make(indexTypeCardinality, Expressions.makeSymbol(numberOfNonAvailableValues));
			}
			else {
				indexNumberOfPossibleValues = makeSymbol(Math.max(0, typeSize - numberOfNonAvailableValues));
			}
			
			indexNumbersOfPossibleValues.add(indexNumberOfPossibleValues);
		}
		
		Expression result = Times.make(indexNumbersOfPossibleValues);
		result = timesRewriter.rewrite(result, process);
		
		return result;
	}

	private Expression getDistinctPredefinedTermForVariable1ThatIsNotVariable2AndIsNotDistinctFromVariable2(
			Collection<Expression> distinctPredefinedTermsForVariable1,
			Collection<Expression> distinctPredefinedTermsForVariable2,
			Expression variable2) {
	
		for (Expression distinctPredefinedTermForVariable1 : distinctPredefinedTermsForVariable1) {
			if ( ! distinctPredefinedTermForVariable1.equals(variable2)) {
				if (distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(distinctPredefinedTermForVariable1, variable2, distinctPredefinedTermsForVariable2)) {
					return distinctPredefinedTermForVariable1;
				}
			}
		}
		return null;
	}

	private boolean distinctPredefinedTermForVariable1IsNotDistinctFromVariable2(
			Expression distinctPredefinedTermForVariable1, Expression variable2,
			Collection<Expression> distinctPredefinedTermsForVariable2) {
	
		if ( ! distinctPredefinedTermsForVariable2.contains(distinctPredefinedTermForVariable1)) {
			Collection<Expression> distinctPredefinedTermsForDistinctPredefinedTermForVariable1 =
					getDistinctPredefinedTermsFrom(distinctPredefinedTermForVariable1);
			if ( ! distinctPredefinedTermsForDistinctPredefinedTermForVariable1.contains(variable2)) {
				return true;
			}
		}
		return false;
	}

	public Collection<Expression> getDistinctPredefinedTermsFrom(Expression variable) {
		Collection<Expression> result = getOrUseDefault(this, variable, emptyList());
		return result;
	}

	public boolean termsAreConstrainedToBeDifferent(Expression term1, Expression term2, RewritingProcess process) {
		boolean result = false;
		if (process.isConstant(term1) && process.isConstant(term2)) {
			result = true;
		}
		else if (getDistinctPredefinedTermsFrom(term1).contains(term2)) {
			result = true;
		}
		else if (getDistinctPredefinedTermsFrom(term2).contains(term1)) {
			result = true;
		}
		return result;
	}
}