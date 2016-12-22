/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.theory.tuple;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter.merge;
import static com.sri.ai.util.Util.set;

import java.util.Arrays;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultCountingFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpllt.theory.equality.SingleVariableEqualityConstraint;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualityTopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleGetSetTopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleQuantifierSimplifier;

/**
 * A {@link Theory) for Tuples.
 * 
 * @author oreilly
 *
 */
@Beta
public class TupleTheory extends AbstractTheoryWithBinaryAtomsIncludingEquality {
	
	private TupleQuantifierSimplifier tupleQuantifierSimplifier = new TupleQuantifierSimplifier();
	
	public TupleTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				set(EQUALITY, DISEQUALITY),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				propagateAllLiteralsWhenVariableIsBound);
	}
	
	@Override
	public TopRewriter makeDefaultTopRewriter() {
		return merge(super.makeDefaultTopRewriter(), new TupleEqualityTopRewriter(), new TupleGetSetTopRewriter());
	}

	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result = isTupleType(type);
		return result;
	}
	
	@Override
	protected boolean isValidArgument(Expression expression, Type type, Context context) {
		boolean result = isTupleType(type);
		return result;
	}
	
	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Theory theory, Context context) {
		return new SingleVariableEqualityConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), theory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {	
		// We return always 'true' (because, if all other theories present are complete, then this one will, too)
		return true;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		// The tuple-specific version will do the following:
		// - create a E = 'there exists X : C' expression equivalent to the satisfiability of the constraint given here.
		Expression exprE = ThereExists.make(Expressions.apply(FunctorConstants.IN, constraint.getVariable(), GrinderUtil.getType(constraint.getVariable(), context)), constraint);
		// - use TupleQuantifierSimplifier to transform it to another expression E' without quantification on tuples
		Expression exprEPrime = tupleQuantifierSimplifier.apply(exprE, context);
		// - return context.getTheory().getRewriter().makeStepSolver(E')		
		ExpressionLiteralSplitterStepSolver result  = context.getTheory().getRewriter().makeStepSolver(exprEPrime);		
		
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		// The tuple-specific version will do the following:
		// - create a E = | X in ... : X | expression equivalent to model counting of the constraint given here.
		Expression exprE = new DefaultCountingFormula(Arrays.asList(Expressions.apply(FunctorConstants.IN, constraint.getVariable(), GrinderUtil.getType(constraint.getVariable(), context))), constraint);
		// - use TupleQuantifierSimplifier to transform it to another expression E' without quantification on tuples
		Expression exprEPrime = tupleQuantifierSimplifier.apply(exprE, context);
		// - return context.getTheory().getRewriter().makeStepSolver(E')
		ExpressionLiteralSplitterStepSolver result  = context.getTheory().getRewriter().makeStepSolver(exprEPrime);
		
		return result;
	}

	@Override
	public 	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Context context) {
		// The tuple-specific version will do the following:
		// - create a E expression equivalent to the quantifier elimination of the constraint given here.
		//          - you can use AssociativeCommutativeGroup.makeProblemExpression(Expression index, Expression indexType, Expression constraint, Expression body)
		//            to create E
		Expression exprE = group.makeProblemExpression(constraint.getVariable(), GrinderUtil.getType(constraint.getVariable(), context), constraint, currentBody);
		// - use TupleQuantifierSimplifier to transform it to another expression E' without quantification on tuples
		Expression exprEPrime = tupleQuantifierSimplifier.apply(exprE, context);
		// - return context.getTheory().getRewriter().makeStepSolver(E')
		ExpressionLiteralSplitterStepSolver result  = context.getTheory().getRewriter().makeStepSolver(exprEPrime);
		
		return result;
	}
	
	private boolean isTupleType(Type type) {
		boolean result = type instanceof TupleType;
		return result;
	}
}
