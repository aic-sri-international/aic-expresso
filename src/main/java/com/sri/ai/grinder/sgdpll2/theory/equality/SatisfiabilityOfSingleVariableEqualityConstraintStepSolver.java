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

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.zipApply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.Util.toLinkedHashSet;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractBooleanProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;
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
 * A {@link AbstractBooleanProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver} for a {@link SingleVariableEqualityConstraint}.
 * <p>
 * This step solver works by providing propagated literals of the form <code>Y op Z</code>
 * for every pair of literals <code>X = Y</code> and <code>X op Z</code>,
 * where <code>X</code> is the constraint's variable and <code>op</code> is either <code>=</code> or </code>!=</code>.
 * <p>
 * It also provides a propagated CNF that encodes the requirement that the number of distinct values
 * from which <code>X</code> is constrained to be disequal from does not exceed the domain size.
 * For example, <code>X != Y and X != Z and X != a</code>, with <code>X</code>'s type {a, b, c, d},
 * propagates the CNF <code>(Y != b or Z = c) and (Y != b or Z != d) and (Y != c or Z != b) and (Y != c or Z != d) and (Y != d and Z != b) and (Y != d and Z != c)</code>.
 * <p>
 * In general, we must generate a CNF with a clause negating each possible assignment of variables disequal from <code>X</code>
 * to a permutation of <code>k</code> distinct uniquely named constants not constrained to be disequal from <code>X</code>,
 * where <code>k</code> is the type size minus the number of uniquely named constants not constrained to be disequal from <code>X</code>.
 * The reason for that is that, if any of these assignments were true, then <code>X</code> would be constrained
 * to be difference from the uniquely named constants it already is constrained against in the constraint itself,
 * plus constrained to be different from <code>k</code> other uniquely named constants,
 * meaning that has to be disequal from a number of uniquely named constants equal to its type's size,
 * leaving no possible assignment to it.
 * <p>
 * While the size of this propagated CNF depends on <code>X</code>'s type size, which can be very large,
 * this check is only needed if the number of terms to which <code>X</code> is constrained to be
 * disequal from is equally large. This preserves
 * the fact that the algorithm's time complexity depends on the constraint size alone,
 * not the type size.
 *
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableEqualityConstraintStepSolver extends AbstractBooleanProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver {

	private boolean alreadyCheckedIfNumberOfDistinctExpressionsIsLessThanStepSolverShouldBeMade = false;
	private NumberOfDistinctExpressionsIsLessThanStepSolver numberOfDistinctExpressionsIsLessThanStepSolver;
	
	public SatisfiabilityOfSingleVariableEqualityConstraintStepSolver(SingleVariableEqualityConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableEqualityConstraint getConstraint() {
		return (SingleVariableEqualityConstraint) super.getConstraint();
	}
	
	private NumberOfDistinctExpressionsIsLessThanStepSolver getNumberOfDistinctExpressionsIsLessThanStepSolver(RewritingProcess process) {
		if ( ! alreadyCheckedIfNumberOfDistinctExpressionsIsLessThanStepSolverShouldBeMade) {
			long variableTypeSize = getConstraint().getVariableTypeSize(process);
			if (variableTypeSize >= 0) {
				numberOfDistinctExpressionsIsLessThanStepSolver = new NumberOfDistinctExpressionsIsLessThanStepSolver((int) variableTypeSize, getConstraint().getDisequals());
			}
			alreadyCheckedIfNumberOfDistinctExpressionsIsLessThanStepSolverShouldBeMade = true;
		}
		return numberOfDistinctExpressionsIsLessThanStepSolver;
	}

	private void setNumberOfDistinctExpressionsIsLessThanStepSolver(NumberOfDistinctExpressionsIsLessThanStepSolver numberOfDistinctExpressionsIsLessThanStepSolver) {
		this.numberOfDistinctExpressionsIsLessThanStepSolver = numberOfDistinctExpressionsIsLessThanStepSolver;
	}
	
	@Override
	protected boolean usingDefaultImplementationOfMakePropagatedCNF() {
		return true;
	}

	@Override
	protected Iterable<Expression> getPropagatedLiterals(RewritingProcess process) {
		
		Iterator<Expression> propagatedLiteralsIterator;

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound()) {
			propagatedLiteralsIterator = new NestedIterator<>(getConstraint().getExternalLiterals());
		}
		else {
			
			Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEqualsToVariableIterator();
			Iterator<Expression> propagatedEqualities = functionIterator(pairsOfEqualsToVariableIterator, p -> Equality.make(p.first, p.second));

			Iterator<Expression> propagatedDisequalities =
					functionIterator(arrayListsOfEqualAndDisequalToVariableIterator(), p -> apply(DISEQUALITY, p));

			propagatedLiteralsIterator =
					new NestedIterator<>(getConstraint().getExternalLiterals(), propagatedEqualities, propagatedDisequalities);
		}
	
		Iterable<Expression> result = in(propagatedLiteralsIterator);
		
		return result;
	}

	protected Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator() {
		PairOfElementsInListIterator<Expression> pairsOfPositiveAtomsIterator = 
				new PairOfElementsInListIterator<>(getConstraint().getPositiveNormalizedAtoms());
		
//		Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = p -> makePairOf(p.first.get(1), p.second.get(1));
// above lambda somehow not working at Ciaran's environment, replacing with seemingly identical anonymous class object below		
		Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = new Function<PairOf<Expression>, PairOf<Expression>>() {
			@Override
			public PairOf<Expression> apply(PairOf<Expression> p) {
				return makePairOf(p.first.get(1), p.second.get(1));
			}
		};
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = functionIterator(pairsOfPositiveAtomsIterator, makePairOfSecondArguments);
		
		return pairsOfEqualsToVariableIterator;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		if ( ! variableIsBoundToUniquelyNamedConstant(process)) {
			// the following logic only holds if the variable is not bound to a uniquely named constants,
			// since that eliminates all disequalities to other uniquely named constants as redundant
	
			long variableDomainSize = getConstraint().getVariableTypeSize(process);
			if (variableDomainSize >= 0 &&
					getConstraint().numberOfDisequals() >= variableDomainSize) {
				// the following procedure can be very expensive but the condition above will rarely be satisfied
	
				ArrayList<Expression> variableDisequals = getVariableDisequals(process);
				Set<Expression> uniquelyNamedConstantDisequals = getUniquelyNamedConstantDisequals(process);
		
				Expression typeExpression = GrinderUtil.getType(getConstraint().getVariable(), process);
				Type type = process.getType(typeExpression);
				ArrayList<Expression> remainingUniquelyNamedConstants =
						arrayListFrom(new PredicateIterator(type.iterator(), c -> ! uniquelyNamedConstantDisequals.contains(c)));
	
				CartesianProductIterator<ArrayList<Expression>> subsetOfVariableDisequalsAndRemainingConstantsPermutationIterator
				= new CartesianProductIterator<ArrayList<Expression>>(
						() -> new SubsetsOfKIterator<Expression>(variableDisequals, remainingUniquelyNamedConstants.size()),
						() -> new PermutationIterator<Expression>(remainingUniquelyNamedConstants));
	
				FunctionIterator<ArrayList<ArrayList<Expression>>, Iterable<Expression>> clausesIterator =
						FunctionIterator.make(
								subsetOfVariableDisequalsAndRemainingConstantsPermutationIterator,
								(ArrayList<ArrayList<Expression>> subsetAndPermutation)
								->
								clauseNegatingAssignmentOfSubsetOfVariablesToParticularPermutationOfRemainingConstants(subsetAndPermutation)
								);
	
				Iterable<Iterable<Expression>> clauses = in(clausesIterator);
	
				return clauses;
			}
		}
	
		// otherwise, nothing is implied.
		return list();
	}

	/**
	 * @param subsetAndPermutation
	 * @return
	 */
	private List<Expression> clauseNegatingAssignmentOfSubsetOfVariablesToParticularPermutationOfRemainingConstants(ArrayList<ArrayList<Expression>> subsetAndPermutation) {
		return zipApply(
				DISEQUALITY,
				list(
						subsetAndPermutation.get(0).iterator(),
						subsetAndPermutation.get(1).iterator()));
	}

	private Iterator<ArrayList<Expression>> arrayListsOfEqualAndDisequalToVariableIterator() {
		
		Function<ArrayList<Expression>, ArrayList<Expression>> extractSecondArguments =
				equalityAndDisequality -> arrayList(equalityAndDisequality.get(0).get(1), equalityAndDisequality.get(1).get(1));
				
		Iterator<ArrayList<Expression>> result =
				FunctionIterator.make(
				new CartesianProductIterator<Expression>(
						() -> getConstraint().getPositiveNormalizedAtoms().iterator(),
						() -> getConstraint().getNegativeNormalizedAtoms().iterator()),
						extractSecondArguments);
		
		return result;
	}

	private boolean variableIsBoundToUniquelyNamedConstant(RewritingProcess process) {
		return thereExists(getConstraint().getPositiveNormalizedAtoms(), l -> process.isUniquelyNamedConstant(l.get(1)));
	}

	private ArrayList<Expression> getVariableDisequals(RewritingProcess process) {
		return getConstraint().getNegativeNormalizedAtoms().stream().
		map(e -> e.get(1)). // second arguments of Variable != Term
		filter(e -> ! process.isUniquelyNamedConstant(e)). // only Variables
		collect(Util.toArrayList(10));
	}

	private LinkedHashSet<Expression> getUniquelyNamedConstantDisequals(RewritingProcess process) {
		return getConstraint().getNegativeNormalizedAtoms().stream().
		map(e -> e.get(1)). // second arguments of Variable != Term
		filter(e -> process.isUniquelyNamedConstant(e)). // only constants
		collect(toLinkedHashSet());
	}

	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Constraint2 contextualConstraint, RewritingProcess process) {
		SolutionStep result;
		if (getNumberOfDistinctExpressionsIsLessThanStepSolver(process) != null) {
			SolutionStep numberStep = getNumberOfDistinctExpressionsIsLessThanStepSolver(process).step(contextualConstraint, process);
			if (numberStep.itDepends()) {
				SatisfiabilityOfSingleVariableEqualityConstraintStepSolver stepSolverIfExpressionIsTrue = clone();
				stepSolverIfExpressionIsTrue.setNumberOfDistinctExpressionsIsLessThanStepSolver((NumberOfDistinctExpressionsIsLessThanStepSolver) numberStep.getStepSolverForWhenLiteralIsTrue());
				
				SatisfiabilityOfSingleVariableEqualityConstraintStepSolver stepSolverIfExpressionIsFalse = clone();
				stepSolverIfExpressionIsFalse.setNumberOfDistinctExpressionsIsLessThanStepSolver((NumberOfDistinctExpressionsIsLessThanStepSolver) numberStep.getStepSolverForWhenLiteralIsFalse());
				
				result = new ItDependsOn(numberStep.getLiteral(), numberStep.getConstraintSplitting(), stepSolverIfExpressionIsTrue, stepSolverIfExpressionIsFalse);
			}
			else {
				result = new Solution(numberStep.getValue());
			}
		}
		else {
			result = new Solution(TRUE);
		}
		return result;
	}
	
	public SatisfiabilityOfSingleVariableEqualityConstraintStepSolver clone() {
		return (SatisfiabilityOfSingleVariableEqualityConstraintStepSolver) super.clone();
	}
}