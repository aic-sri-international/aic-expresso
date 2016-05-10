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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.grinder.helper.GrinderUtil.REAL_TYPE;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.group.SymbolicPlusGroup;
import com.sri.ai.grinder.sgdpll.problemtype.SumProduct;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.DefaultMapBasedTopSimplifier;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.numeric.AbstractNumericConstraintTheory;


/** 
 * A {@link ConstraintTheory} for linear real arithmetic literals.
 */
@Beta
public class LinearRealArithmeticConstraintTheory extends AbstractNumericConstraintTheory {

	/**
	 * Creates an linear real arithmetic theory for integers.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundConstraintTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound whether literals on a variable bound to a term should be immediately replaced by a literal on that term instead.
	 */
	public LinearRealArithmeticConstraintTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, 
				propagateAllLiteralsWhenVariableIsBound, 
				new DefaultMapBasedTopSimplifier(
						makeFunctionApplicationSimplifiersForLinearRealArithmeticConstraintTheory(), 
						map()));
		// It's important to include the different arithmetic simplifier to avoid leaving linear real arithmetic literals that could be picked up as splitters,
		// but actually contain variables that cancel out, with the result of the literal becoming a boolean constant unfit to be splitter.
	}
	
	private static Map<String, Simplifier> makeFunctionApplicationSimplifiersForLinearRealArithmeticConstraintTheory() {
		Simplifier linearRealArithmeticSimplifier = new LinearRealSimplifier();
		Map<String, Simplifier> functionApplicationSimplifiers =
				map(
						EQUALITY,                 linearRealArithmeticSimplifier,
						DISEQUALITY,              linearRealArithmeticSimplifier,
						LESS_THAN,                linearRealArithmeticSimplifier,
						LESS_THAN_OR_EQUAL_TO,    linearRealArithmeticSimplifier,
						GREATER_THAN,             linearRealArithmeticSimplifier,
						GREATER_THAN_OR_EQUAL_TO, linearRealArithmeticSimplifier
						);

		return functionApplicationSimplifiers;
	}

	@Override
	protected void initializeTestingInformation() {
		String typeName = "[0;4]";
		RealInterval type = new RealInterval(typeName);
		setVariableNamesAndTypesForTesting(map("X", type, "Y", type, "Z", type));
	}

	@Override
	protected boolean isValidArgument(Expression expression, Type type) {
		Expression parsedType = Expressions.parse(type.toString());
		boolean result = 
				parsedType.equals("Real") 
				|| 
				(isRealInterval(parsedType) && parsedType.numberOfArguments() == 2);
		return result;
	}

	/**
	 * @param parsedType
	 * @return
	 */
	public boolean isRealInterval(Expression parsedType) {
		return 
				parsedType.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_OPEN)
				||
				parsedType.hasFunctor(FunctorConstants.REAL_INTERVAL_OPEN_CLOSED)
				||
				parsedType.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_OPEN)
				||
				parsedType.hasFunctor(FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED);
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, Context context) {
		return new SingleVariableLinearRealArithmeticConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), constraintTheory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return false; // SingleVariableLinearRealArithmeticConstraint is not complete
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		return new SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) constraint);
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		return new ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) constraint);
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Simplifier simplifier, Context context) {
		ContextDependentExpressionProblemStepSolver result;
		if (group instanceof SymbolicPlusGroup || group instanceof SumProduct) {
			result = new SummationOnLinearRealArithmeticAndPolynomialStepSolver(constraint, currentBody, simplifier);
		}
		else {
			result = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver
					(group, simplifier, constraint, currentBody);
		}
		return result;
	}

	/**
	 * Makes a random atom on variable by summing or subtracting terms from two random atoms generated by super class implementation.
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Random random, Context context) {
		
		return null;
		
		// TODO: write this method
	}
	
	/**
	 * This is overridden to
	 * add an instance of {@link RealExpressoType} to testing types as well.
	 * This is needed because arithmetic expressions such as J + 5 are determined to be of
	 * type name "Real" by {@link GrinderUtil#getType(Expression expression, Context context)},
	 * so a type with this name is needed by the default implementation of {@link #isNonTrivialAtom(Expression, Context)}
	 * if the flag for analyzing the types of arguments to equalities is true.
	 */
	@Override
	public Collection<Type> getNativeTypes() {
		return list(REAL_TYPE);
	}
}