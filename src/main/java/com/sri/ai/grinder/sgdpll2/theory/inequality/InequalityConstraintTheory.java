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
package com.sri.ai.grinder.sgdpll2.theory.inequality;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.INTEGER_TYPE;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.INTEGER_INTERVAL;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.pickKElementsWithoutReplacement;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.RecursiveExhaustiveSeriallyMergedMapBasedSimplifier;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.library.number.NumericSimplifier;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.plaindpll.group.SymbolicPlusGroup;
import com.sri.ai.grinder.plaindpll.problemtype.SumProduct;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.solver.QuantifierEliminationOnBodyWithIndexInLiteralsOnlyStepSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.SummationOnIntegerInequalityAndPolynomialStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractConstraintTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.helper.DifferenceArithmeticSimplifier;
import com.sri.ai.util.Util;


/** 
 * A {@link ConstraintTheory} for integer inequality literals.
 */
@Beta
public class InequalityConstraintTheory extends AbstractConstraintTheoryWithBinaryAtomsIncludingEquality {

	static final Map<String, String> negationFunctor =
	Util.map(
			EQUALITY,                 DISEQUALITY,
			DISEQUALITY,              EQUALITY,
			LESS_THAN,                GREATER_THAN_OR_EQUAL_TO,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN,
			GREATER_THAN,             LESS_THAN_OR_EQUAL_TO,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN
			);

//	/**
//	 * 	 * Creates an inequality theory for integers that does <i>not</i> assume equality literals are literal of this theory
//	 * (this is more expensive -- use for a more efficiency setting if all equalities belong to this theory).
//	 */
//	public InequalityConstraintTheory(boolean propagateAllLiteralsWhenVariableIsBound) {
//		this(false, propagateAllLiteralsWhenVariableIsBound);
//	}
	
	/**
	 * Creates an inequality theory for integers.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundConstraintTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 */
	public InequalityConstraintTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				negationFunctor.keySet(),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				new RecursiveExhaustiveSeriallyMergedMapBasedSimplifier(
						// it is important to include difference arithmetic simplifiers here because they ensure literals that contain a variable that cancels out (such as X - X > Y) are simplified (here, to 0 > Y) and as a consequence not passed to the single-variable constraint for that variable (here, X), because it is actually *not* a constraint on X
						makeFunctionApplicationSimplifiersForDifferenceArithmetic(),
						map(), // no additional syntactic form simplifiers

						// basic simplification of involved interpreted functions in this theory:
						new EqualitySimplifier(),
						new InequalitySimplifier(),
						new BooleanSimplifier(),
						new NumericSimplifier()
						),
						propagateAllLiteralsWhenVariableIsBound);

		String typeName = "0..4";
		IntegerInterval type = new IntegerInterval(typeName);
		setVariableNamesAndTypesForTesting(map("I", type, "J", type, "K", type));
	}
	
	private static Map<String, Simplifier> makeFunctionApplicationSimplifiersForDifferenceArithmetic() {
		Simplifier differenceArithmeticSimplifier = new DifferenceArithmeticSimplifier();
		Map<String, Simplifier> functionApplicationSimplifiers =
				map(
						EQUALITY,                 differenceArithmeticSimplifier,
						DISEQUALITY,              differenceArithmeticSimplifier,
						LESS_THAN,                differenceArithmeticSimplifier,
						LESS_THAN_OR_EQUAL_TO,    differenceArithmeticSimplifier,
						GREATER_THAN,             differenceArithmeticSimplifier,
						GREATER_THAN_OR_EQUAL_TO, differenceArithmeticSimplifier
						);

		// Note that this may lead equalities and disequalities *not* on integers to be passed to this simplifier, but it does not make changes to those and may actually even non-integer terms out.
		
		return functionApplicationSimplifiers;
	}
	
	@Override
	protected boolean isValidArgument(Expression expression, Type type) {
		Expression parsedType = Expressions.parse(type.toString());
		boolean result = parsedType.equals("Integer") || (parsedType.hasFunctor(INTEGER_INTERVAL) && parsedType.numberOfArguments() == 2);
		return result;
	}

	@Override
	protected Expression getNonTrivialAtomNegation(Expression atom) {
		String functorString = atom.getFunctor().toString();
		String negatedFunctor = negationFunctor.get(functorString);
		Expression result = apply(negatedFunctor, atom.get(0), atom.get(1));
		return result;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		boolean result = super.isInterpretedInThisTheoryBesidesBooleanConnectives(expression, process)
				|| expression.equals(PLUS) || expression.equals(MINUS)
				|| expression.hasFunctor(PLUS) || expression.hasFunctor(MINUS); 
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process) {
		return new SingleVariableInequalityConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), constraintTheory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return false; // SingleVariableInequalityConstraint is not complete
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new SatisfiabilityOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new ModelCountingOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
	}

	@Override
	public 	ContextDependentExpressionProblemStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Simplifier simplifier, RewritingProcess process) {
		ContextDependentExpressionProblemStepSolver result;
		if (group instanceof SymbolicPlusGroup || group instanceof SumProduct) {
			result = new SummationOnIntegerInequalityAndPolynomialStepSolver(constraint, currentBody, simplifier);
		}
		else {
			result = new QuantifierEliminationOnBodyWithIndexInLiteralsOnlyStepSolver
					(group, simplifier, constraint, currentBody);
		}
		return result;
	}

	/**
	 * Makes a random atom on variable by summing or subtracting terms from two random atoms generated by super class implementation.
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
		
		int maxNumberOfOtherVariablesInAtom = Math.min(getVariableNamesForTesting().size(), 2);
		int numberOfOtherVariablesInAtom = random.nextInt(maxNumberOfOtherVariablesInAtom); // used to be 3, but if literal has more than two variables, it steps out of difference arithmetic and may lead to multiplied variables when literals are propagated. For example, X = Y + Z and X = -Y - Z + 3 imply 2Y + 2Z = 3 
		ArrayList<String> otherVariablesForAtom = pickKElementsWithoutReplacement(new ArrayList<>(getVariableNamesForTesting()), numberOfOtherVariablesInAtom, o -> !o.equals(variable), random);
		// Note that otherVariablesForAtom contains only one or zero elements
		
		Type type = getVariableNamesAndTypesForTesting().get(variable);
		ArrayList<Expression> constants = new ArrayList<Expression>();
		int numberOfConstants = random.nextInt(3);
		for (int i = 0; i != numberOfConstants; i++) {
			Expression sampledConstant = type.sampleUniquelyNamedConstant(random);
			Expression constant;
			if (random.nextBoolean()) {
				constant = sampledConstant;
			}
			else {
				constant = makeSymbol(-sampledConstant.intValue());
			}
			constants.add(constant);
		}

		ArrayList<Expression> leftHandSideArguments = new ArrayList<Expression>();
		leftHandSideArguments.add(makeSymbol(variable));
		Util.mapIntoList(otherVariablesForAtom, s -> UnaryMinus.make(makeSymbol(s)), leftHandSideArguments); // needs to be difference, so it's added as negative
		leftHandSideArguments.addAll(constants);

		int numberOfOtherVariablesToBeCanceled = random.nextInt(otherVariablesForAtom.size() + 1);
		ArrayList<String> otherVariablesToBeCanceled = Util.pickKElementsWithoutReplacement(otherVariablesForAtom, numberOfOtherVariablesToBeCanceled, random);
		Util.mapIntoList(otherVariablesToBeCanceled, v -> makeSymbol(v), leftHandSideArguments); // note that this term is positive, so it will cancel the previously negative term with the same "other variable"
		// it may seem odd to generate an "other variable" and add another term that will cancel it later. However, this is useful for making sure canceling works properly.
		
		Expression leftHandSide = Plus.make(leftHandSideArguments);
		String functor = pickUniformly(theoryFunctors, random);
		Expression unsimplifiedResult = apply(functor, leftHandSide, 0);
		
		Expression result = simplify(unsimplifiedResult, process);
		//System.out.println("Random literal: " + result);	
		// Note that simplify will eliminate negated variables;
		// however, we leave their generation and then elimination here as a sanity check,
		// as well as a useful feature for the day when we get assurance that literals will be simplified down the line,
		// allowing us to eliminate them here. TODO
		
		return result;
	}
	
	/**
	 * This is overridden to
	 * add an instance of {@link IntegerExpressoType} to testing types as well.
	 * This is needed because arithmetic expressions such as J + 5 are determined to be of
	 * type name "Integer" by {@link GrinderUtil#getType(Expression expression, RewritingProcess process)},
	 * so a type with this name is needed by the default implementation of {@link #isNonTrivialAtom(Expression, RewritingProcess)}
	 * if the flag for analyzing the types of arguments to equalities is true.
	 */
	@Override
	public Collection<Type> getNativeTypes() {
		return list(INTEGER_TYPE);
	}
}