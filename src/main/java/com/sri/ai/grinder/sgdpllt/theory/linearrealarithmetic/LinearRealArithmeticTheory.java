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
package com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic;

import static com.sri.ai.grinder.helper.GrinderUtil.REAL_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.grinder.sgdpllt.rewriter.core.Switch.FUNCTOR;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.numeric.AbstractNumericTheory;

/** 
 * A {@link Theory} for linear real arithmetic literals.
 */
@Beta
public class LinearRealArithmeticTheory extends AbstractNumericTheory {

	/**
	 * Creates an linear real arithmetic theory.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundTheory} mixing multiple theories involving
	 * equalities is being used.
	 * <p>
	 * Testing information is initialized to variables <code>X</code>, <code>Y</code>, <code>Z</code> in interval <code>[0;4]</code>.
	 * 
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound whether literals on a variable bound to a term should be immediately replaced by a literal on that term instead.
	 */
	public LinearRealArithmeticTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, 
				propagateAllLiteralsWhenVariableIsBound);
	}
	
	@Override
	public TopRewriter getDefaultTopRewriter() {
		// It's important to include the difference arithmetic simplifier to avoid leaving DA literals that could be picked up as splitters,
		// but actually contain variables that cancel out (for example, X - X = 0),
		// with the result of the literal becoming a boolean constant unfit to be splitter.
		Simplifier linearRealArithmeticSimplifier = new LinearRealArithmeticSimplifier(this);
		return 
				TopRewriter.merge(
						super.getDefaultTopRewriter(),
						new Switch<>(
								FUNCTOR,
								map(
										EQUALITY,                 linearRealArithmeticSimplifier,
										DISEQUALITY,              linearRealArithmeticSimplifier,
										LESS_THAN,                linearRealArithmeticSimplifier,
										LESS_THAN_OR_EQUAL_TO,    linearRealArithmeticSimplifier,
										GREATER_THAN,             linearRealArithmeticSimplifier,
										GREATER_THAN_OR_EQUAL_TO, linearRealArithmeticSimplifier
								)
						)
				);
	}

	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result = 
				type instanceof RealExpressoType || 
				type instanceof RealInterval;
		return result;
	}

	@Override
	protected boolean isValidArgument(Expression expression, Type type, Context context) {
		boolean result = 
				type instanceof RealExpressoType || 
				type instanceof RealInterval ||
				((type instanceof IntegerExpressoType || type instanceof IntegerInterval)
						&&
						getVariablesIn(expression, context).isEmpty()
						);
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Theory theory, Context context) {
		return new SingleVariableLinearRealArithmeticConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), theory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return false; // SingleVariableLinearRealArithmeticConstraint is not complete
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = new SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) constraint);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = new ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) constraint);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression body, Context context) {
		ExpressionStepSolver formulaSplitterStepSolver;
		if (group instanceof Sum) {
			formulaSplitterStepSolver = new SummationOnLinearRealArithmeticAndPolynomialStepSolver(constraint, body);
		}
		else {
			formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver
					(group, constraint, body);
		}
		ExpressionLiteralSplitterStepSolver result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSplitterStepSolver);
		return result;
	}
	
	/**
	 * This is overridden to
	 * add an instance of {@link RealExpressoType} to testing types as well.
	 * This is needed because arithmetic expressions such as J + 5 are determined to be of
	 * type name "Real" by {@link GrinderUtil#getTypeExpressionOfExpression(Expression expression, Registry registry)},
	 * so a type with this name is needed by the default implementation of {@link #isAtom(Expression, Context)}
	 * if the flag for analyzing the types of arguments to equalities is true.
	 */
	@Override
	public Collection<Type> getNativeTypes() {
		return list(REAL_TYPE);
	}

	@Override
	/**
	 * Extends super implementation by considering * applications as interpreted in this theory.
	 */
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		boolean result = super.isInterpretedInThisTheoryBesidesBooleanConnectives(expression)
				|| expression.equals(TIMES)
				|| expression.hasFunctor(TIMES); 
		return result;
	}
}