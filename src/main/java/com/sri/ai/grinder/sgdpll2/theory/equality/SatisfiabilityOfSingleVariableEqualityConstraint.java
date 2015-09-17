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
package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.zipApply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.Util.toLinkedHashSet;
import static com.sri.ai.util.base.PairOf.makePairOf;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;
import com.sri.ai.util.collect.PermutationIterator;
import com.sri.ai.util.collect.PredicateIterator;
import com.sri.ai.util.collect.SubsetsOfKIterator;

/**
 * A {@link AbstractSatisfiabilityOfConstraint} for a {@link SingleVariableEqualityConstraint}.
 * 
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableEqualityConstraint extends AbstractSatisfiabilityOfConstraint {

	public SatisfiabilityOfSingleVariableEqualityConstraint(SingleVariableEqualityConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) constraint;
	}
	
	@Override
	protected Iterable<Expression> propagatedLiterals() {
		
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEqualsToVariableIterator();
		Iterator<Expression> propagatedEqualities = FunctionIterator.make(pairsOfEqualsToVariableIterator, p -> Equality.make(p.first, p.second));
		
		Iterator<Expression> propagatedDisequalities =
				FunctionIterator.make(arrayListsOfEqualAndDisequalToVariableIterator(), p -> Expressions.apply(DISEQUALITY, p));
		
		Iterator<Expression> propagatedLiteralsIterator = new NestedIterator<>(propagatedEqualities, propagatedDisequalities);

		Iterable<Expression> result = in(propagatedLiteralsIterator);
		
		return result;
	}

	protected Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator() {
		PairOfElementsInListIterator<Expression> pairsOfPositiveAtomsIterator = new PairOfElementsInListIterator<>(getConstraint().getPositiveAtoms());
		
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator =
				FunctionIterator.make(pairsOfPositiveAtomsIterator, new Function<PairOf<Expression>, PairOf<Expression>>() {
					@Override
					public PairOf<Expression> apply(PairOf<Expression> p) {
						return makePairOf(p.first.get(1), p.second.get(1));
					}
				});
		
		return pairsOfEqualsToVariableIterator;
	}

	@SuppressWarnings("unchecked")
	protected Iterator<ArrayList<Expression>> arrayListsOfEqualAndDisequalToVariableIterator() {
		
		Function<ArrayList<Expression>, ArrayList<Expression>> extractSecondArguments =
				equalityAndDisequality -> arrayList(equalityAndDisequality.get(0).get(1), equalityAndDisequality.get(1).get(1));
				
		Iterator<ArrayList<Expression>> result =
				FunctionIterator.make(
				new CartesianProductIterator<Expression>(
						() -> getConstraint().getPositiveAtoms().iterator(),
						() -> getConstraint().getNegativeAtoms().iterator()),
						extractSecondArguments);
		
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNF(RewritingProcess process) {
		if ( ! variableIsBoundToUniquelyNamedConstant(process)) {
			// the following logic only holds if the variable is not bound to a uniquely named constants,
			// since that eliminates all disequalities to other uniquely named constants as redundant

			ArrayList<Expression> variableDisequals = getVariableDisequals(process);

			if (getConstraint().getVariableDomainSize(process) != -1 &&
					variableDisequals.size() + getConstraint().getNumberOfDisequalitiesFromConstantsSeenSoFar()
					>= getConstraint().getVariableDomainSize(process)) {
				// the following procedure can be very expensive but the condition above will rarely be satisfied

				Set<Expression> constantDisequals = getConstantDisequals(process);

				Expression typeDescription = GrinderUtil.getType(getConstraint().getVariable(), process);
				Type type = process.getType(typeDescription.toString());
				ArrayList<Expression> remainingConstants =
						arrayListFrom(new PredicateIterator(type.iterator(), c -> ! constantDisequals.contains(c)));

				CartesianProductIterator<ArrayList<Expression>> subsetOfVariableDisequalsAndRemainingConstantsPermutationIterator
				= new CartesianProductIterator<ArrayList<Expression>>(
						() -> new SubsetsOfKIterator<Expression>(variableDisequals, remainingConstants.size()),
						() -> new PermutationIterator<Expression>(remainingConstants));

				FunctionIterator<ArrayList<ArrayList<Expression>>, Iterable<Expression>> disjunctsIterator =
						FunctionIterator.make(
								subsetOfVariableDisequalsAndRemainingConstantsPermutationIterator,
								(ArrayList<ArrayList<Expression>> subsetAndPermutation)
								->
								zipApply(
										DISEQUALITY,
										list(
												subsetAndPermutation.get(0).iterator(),
												subsetAndPermutation.get(1).iterator()))
								);

				Iterable<Iterable<Expression>> conjuncts = in(disjunctsIterator);

				return conjuncts;
			}
		}

		
		// otherwise, nothing is implied.
		return list();
	}

	/**
	 * @param process
	 * @return
	 */
	private boolean variableIsBoundToUniquelyNamedConstant(RewritingProcess process) {
		return thereExists(getConstraint().getPositiveAtoms(), l -> process.isUniquelyNamedConstant(l.get(1)));
	}

	/**
	 * @param process
	 * @return
	 */
	private ArrayList<Expression> getVariableDisequals(RewritingProcess process) {
		return getConstraint().getNegativeAtoms().stream().
		map(e -> e.get(1)). // second arguments of Variable != Term
		filter(e -> ! process.isUniquelyNamedConstant(e)). // only Variables
		collect(Util.toArrayList(10));
	}

	/**
	 * @param process
	 * @return
	 */
	private LinkedHashSet<Expression> getConstantDisequals(RewritingProcess process) {
		return getConstraint().getNegativeAtoms().stream().
		map(e -> e.get(1)). // second arguments of Variable != Term
		filter(e -> process.isUniquelyNamedConstant(e)). // only constants
		collect(toLinkedHashSet());
	}
}