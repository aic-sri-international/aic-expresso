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
package com.sri.ai.grinder.sgdpll.theory.compound;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.check;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractConstraintTheory;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.RecursiveExhaustiveSeriallyMergedMapBasedSimplifier;
import com.sri.ai.util.Util;

/** 
 * A {@link ConstraintTheory} formed by the union of other constraint theories.
 */
@Beta
public class CompoundConstraintTheory extends AbstractConstraintTheory {

	private List<ConstraintTheory> subConstraintTheories;
	
	public CompoundConstraintTheory(ConstraintTheory... subConstraintTheoriesArray) {
		super(makeSimplifier(list(subConstraintTheoriesArray)));
		this.subConstraintTheories = list(subConstraintTheoriesArray);
		Util.myAssert(() -> subConstraintTheories.size() != 0, () -> getClass() + " needs to receive at least one sub-constraint theory but got none.");
		aggregateTestingInformation();
	}

	private void aggregateTestingInformation() throws Error {
		Map<String, Type> variableNamesAndTypesForTesting = new LinkedHashMap<>();
		for (ConstraintTheory constraintTheory : getSubConstraintTheories()) {
			Set<Entry<String, Type>> variableNamesAndTypeNameEntries = constraintTheory.getVariableNamesAndTypesForTesting().entrySet();
			for (Map.Entry<String, Type> variableNameAndTypeName : variableNamesAndTypeNameEntries) {
				String variableName = variableNameAndTypeName.getKey();
				Type type = variableNameAndTypeName.getValue();
				if (variableNamesAndTypesForTesting.containsKey(variableName)) {
					throw new Error("Variable " + variableName + " is a testing variable for " + constraintTheory.getClass() + " added to a compound constraint theory, but had already been registered by a previous sub-constraint theory.");
				}
				else {
					variableNamesAndTypesForTesting.put(variableName, type);
				}
			}
		}
		setVariableNamesAndTypesForTesting(variableNamesAndTypesForTesting);
	}
	
	/**
	 * This is overridden so that given variables and types for testing are distributed to their
	 * respective theories according to {@link #isSuitableFor(Expression, Type)}.
	 */
	@Override
	public void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		
		Map<ConstraintTheory, Map<String, Type>> mapForSubConstraintTheory = map();
		
		if (getSubConstraintTheories() != null) { // during construction, these may not be available yet.
			for (ConstraintTheory subConstraintTheory : getSubConstraintTheories()) {
				mapForSubConstraintTheory.put(subConstraintTheory, map());
			}
	
			for (Map.Entry<String, Type> variableNameAndType : variableNamesAndTypesForTesting.entrySet()) {
				String variableName = variableNameAndType.getKey();
				Expression variable = Expressions.parse(variableName);
				Type type = variableNameAndType.getValue();
				for (ConstraintTheory subConstraintTheory : getSubConstraintTheories()) {
					if (subConstraintTheory.isSuitableFor(variable, type)) {
						mapForSubConstraintTheory.get(subConstraintTheory).put(variableName, type);
					}
				}
			}
	
			for (ConstraintTheory subConstraintTheory : getSubConstraintTheories()) {
				Map<String, Type> forThisSubTheory = mapForSubConstraintTheory.get(subConstraintTheory);
				subConstraintTheory.setVariableNamesAndTypesForTesting(forThisSubTheory);
			}
		}
	
		super.setVariableNamesAndTypesForTesting(variableNamesAndTypesForTesting);
	}

	@Override
	public Collection<Type> getNativeTypes() {
		Collection<Type> result = new LinkedHashSet<Type>();
		for (ConstraintTheory subConstraintTheory : getSubConstraintTheories()) {
			result.addAll(subConstraintTheory.getNativeTypes());
		}
		return result;
	}

	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result = thereExists(getSubConstraintTheories(), t -> t.isSuitableFor(variable, type));
		return result;
	}
	
	public Collection<ConstraintTheory> getSubConstraintTheories() {
		return subConstraintTheories;
	}

	private ConstraintTheory getConstraintTheory(Expression variable, RewritingProcess process) {
		String typeName = GrinderUtil.getType(variable, process).toString();
		Type type = process.getType(typeName);
		
		ConstraintTheory result =
				getFirstSatisfyingPredicateOrNull(
						getSubConstraintTheories(),
						t -> t.isSuitableFor(variable, type));

		check(() -> result != null, () -> "There is no sub-constraint theory suitable for " + variable + ", which has type " + GrinderUtil.getType(variable, process));
		
		return result;
		
//		Expression type = GrinderUtil.getType(variable, process);
//		if (type == null) {
//			throw new Error(getClass() + " could not identify the type of " + variable + " and therefore not its sub-constraint theory either.");
//		}
//		else {
//			return fromTypeToConstraintTheories.get(type.toString());
//		}
	}
	
	@Override
	public boolean isNonTrivialAtom(Expression expression, RewritingProcess process) {
		boolean result = thereExists(getSubConstraintTheories(), t -> t.isNonTrivialAtom(expression, process));
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process) {
		ConstraintTheory constraintTheoryForVariable = getConstraintTheory(variable, process);
		SingleVariableConstraint result = constraintTheoryForVariable.makeSingleVariableConstraint(variable, constraintTheory, process);
		return result;
	}

	private int cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = -1; // int so we can know whether it is 'undefined'
	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		if (cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == -1) {
			boolean trueForAllTheories =
					forAll(
							getSubConstraintTheories(),
							ConstraintTheory::singleVariableConstraintIsCompleteWithRespectToItsVariable);
			cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = trueForAllTheories ? 1 : 0;
		}
		return cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == 1;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		boolean result =
				thereExists(
						getSubConstraintTheories(),
						t -> t.isInterpretedInThisTheoryBesidesBooleanConnectives(expression, process));
		return result;
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(constraint.getVariable(), process);
		ContextDependentExpressionProblemStepSolver result = constraintTheory.getSingleVariableConstraintSatisfiabilityStepSolver(constraint, process);
		return result;
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(constraint.getVariable(), process);
		ContextDependentExpressionProblemStepSolver result = constraintTheory.getSingleVariableConstraintModelCountingStepSolver(constraint, process);
		return result;
	}

	@Override
	public 	ContextDependentExpressionProblemStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Simplifier simplifier, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(constraint.getVariable(), process);
		ContextDependentExpressionProblemStepSolver result = constraintTheory.getSingleVariableConstraintQuantifierEliminatorStepSolver(group, constraint, currentBody, simplifier, process);
		return result;
	}

	@Override
	public Expression getLiteralNegation(Expression literal, RewritingProcess process) {
		ConstraintTheory constraintTheory =
				getFirstSatisfyingPredicateOrNull(getSubConstraintTheories(), t -> t.isLiteral(literal, process));
		Expression result = constraintTheory.getLiteralNegation(literal, process);
		return result;
	}

	@Override
	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(parse(variable), process);
		Expression result = constraintTheory.makeRandomAtomOn(variable, random, process);
		return result;
	}

	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	private static MapBasedSimplifier makeSimplifier(Collection<ConstraintTheory> subConstraintTheories) {
		
		MapBasedSimplifier[] subSimplifiers
		= mapIntoArray(MapBasedSimplifier.class, subConstraintTheories, t -> t.getTopSimplifier());
		
		
		MapBasedSimplifier simplifier
		= new RecursiveExhaustiveSeriallyMergedMapBasedSimplifier(subSimplifiers);
		
		return simplifier;
	}

	@Override
	public String toString() {
		return "Compound constraint theory (" + join(getSubConstraintTheories()) + ")";
	}
}