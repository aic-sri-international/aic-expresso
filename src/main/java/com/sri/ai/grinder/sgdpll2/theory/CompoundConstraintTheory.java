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
package com.sri.ai.grinder.sgdpll2.theory;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.Exhaustive;
import com.sri.ai.grinder.core.simplifier.Pipeline;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractConstraintTheory;
import com.sri.ai.util.Util;

/** 
 * A {@link ConstraintTheory} formed by the union of other constraint theories.
 */
@Beta
public class CompoundConstraintTheory extends AbstractConstraintTheory {

	private Map<String, ConstraintTheory> fromTypeToConstraintTheories;
	
	public CompoundConstraintTheory(Object... typeStringsAndSubConstraintTheories) {
		this.fromTypeToConstraintTheories = map(typeStringsAndSubConstraintTheories);
		Util.myAssert(() -> fromTypeToConstraintTheories.size() != 0, () -> getClass() + " needs to receive at least one sub-constraint theory but got none.");
		aggregateTestingInformation();
	}

	private void aggregateTestingInformation() throws Error {
		Collection<Type> typesForTesting = new LinkedHashSet<>();
		for (ConstraintTheory constraintTheory : getConstraintTheories()) {
			typesForTesting.addAll(constraintTheory.getTypesForTesting());
		}
		setTypesForTesting(typesForTesting);
		
		Map<String, String> variableNamesAndTypeNamesForTesting = new LinkedHashMap<>();
		for (ConstraintTheory constraintTheory : getConstraintTheories()) {
			Set<Entry<String, String>> variableNamesAndTypeNameEntries = constraintTheory.getVariableNamesAndTypeNamesForTesting().entrySet();
			for (Map.Entry<String, String> variableNameAndTypeName : variableNamesAndTypeNameEntries) {
				String variableName = variableNameAndTypeName.getKey();
				String typeName = variableNameAndTypeName.getValue();
				if (variableNamesAndTypeNamesForTesting.containsKey(variableName)) {
					throw new Error("Variable " + variableName + " is a testing variable for " + constraintTheory.getClass() + " added to a compound constraint theory, but had already been registered by a previous sub-constraint theory.");
				}
				else {
					variableNamesAndTypeNamesForTesting.put(variableName, typeName);
				}
			}
		}
		setVariableNamesAndTypeNamesForTesting(variableNamesAndTypeNamesForTesting);
		
		ConstraintTheory firstConstraintTheory = Util.getFirst(getConstraintTheories());
		this.setTestingVariable(firstConstraintTheory.getTestingVariable());
	}
	
	public Collection<ConstraintTheory> getConstraintTheories() {
		return fromTypeToConstraintTheories.values();
	}

	private ConstraintTheory getConstraintTheory(Expression variable, RewritingProcess process) {
		Expression type = GrinderUtil.getType(variable, process);
		if (type == null) {
			throw new Error(getClass() + " could not identify the type of " + variable + " and therefore not its sub-constraint theory either.");
		}
		else {
			return fromTypeToConstraintTheories.get(type.toString());
		}
	}
	
	@Override
	public boolean isNonTrivialLiteral(Expression expression, RewritingProcess process) {
		boolean result = thereExists(getConstraintTheories(), t -> t.isNonTrivialLiteral(expression, process));
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, RewritingProcess process) {
		ConstraintTheory constraintTheoryForVariable = getConstraintTheory(variable, process);
		SingleVariableConstraint result = constraintTheoryForVariable.makeSingleVariableConstraint(variable, process);
		return result;
	}

	private int cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = -1; // int so we can know whether it is 'undefined'
	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		if (cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == -1) {
			boolean trueForAllTheories =
					forAll(
							getConstraintTheories(),
							ConstraintTheory::singleVariableConstraintIsCompleteWithRespectToItsVariable);
			cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = trueForAllTheories ? 1 : 0;
		}
		return cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == 1;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		boolean result =
				thereExists(
						getConstraintTheories(),
						t -> t.isInterpretedInThisTheoryBesidesBooleanConnectives(expression, process));
		return result;
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(constraint.getVariable(), process);
		ContextDependentProblemStepSolver result = constraintTheory.getSingleVariableConstraintSatisfiabilityStepSolver(constraint, process);
		return result;
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraintTheory(constraint.getVariable(), process);
		ContextDependentProblemStepSolver result = constraintTheory.getSingleVariableConstraintModelCountingStepSolver(constraint, process);
		return result;
	}

	@Override
	public Expression getLiteralNegation(Expression literal, RewritingProcess process) {
		ConstraintTheory constraintTheory =
				getFirstSatisfyingPredicateOrNull(getConstraintTheories(), t -> t.isLiteral(literal, process));
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
		Collection<Simplifier> simplifiers = mapIntoList(getConstraintTheories(), t -> (e, p) -> t.simplify(e, p));
		Simplifier exhaustivelyOfAll = new Exhaustive(new Pipeline(simplifiers));
		Expression result = exhaustivelyOfAll.apply(expression, process);
		return result;
	}
}