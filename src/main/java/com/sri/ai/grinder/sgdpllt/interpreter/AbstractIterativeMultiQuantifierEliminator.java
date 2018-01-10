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
package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.util.Util.in;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractMultiQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;

/**
 * An abstract class for quantifier eliminators using a simple (total or sampled)
 * iteration over the domain of the eliminated variables.
 * <p>
 * This abstract class implements the procedure for group aggregating (for example, summing) over
 * the values of a summand provided by the extending class,
 * after it is evaluated under the assignments also provided by the extending class.
 * <p>
 * The current assignment to the eliminated variables is kept in the {@link Context}'s
 * global objects, and accessible through {@link ContextAssignmentLookup}.
 * This assignment can be extended with {@link Assignment#extendAssignments(Map, Context)}.
 * This same assignment is also used by top rewriters in implementations of
 * {@link AbstractInterpreter} to replace variables by their values.
 * <p>
 * 
 * @author braz
 *
 */
public abstract class AbstractIterativeMultiQuantifierEliminator extends AbstractMultiQuantifierEliminator {

	protected TopRewriterUsingContextAssignments topRewriterUsingContextAssignments;

	/**
	 * Make the term to be summed for all assignments provided by assignments iterator.
	 */
	public abstract Expression makeSummand(MultiQuantifierEliminationProblem problem, Context context);

	/**
	 * Makes an iterator (ranging assignments from indices to their values)
	 * that will be used to generate all the terms to be added.
	 * @param indices
	 * @param indicesCondition
	 * @param context
	 * @return
	 */
	public abstract Iterator<Assignment> makeAssignmentsIterator(List<Expression> indices, Expression indicesCondition, Context context);

	public AbstractIterativeMultiQuantifierEliminator(TopRewriter topRewriter) {
		this(new TopRewriterUsingContextAssignmentsReceivingBaseTopRewriterAtConstruction(topRewriter));
	}

	public AbstractIterativeMultiQuantifierEliminator(TopRewriterUsingContextAssignments topRewriterUsingContextAssignments) {
		super();
		this.topRewriterUsingContextAssignments = topRewriterUsingContextAssignments;
	}

	public TopRewriterUsingContextAssignments getTopRewriterUsingContextAssignments() {
		return topRewriterUsingContextAssignments;
	}
	
	@Override
	public Expression extendContextAndSolve(AssociativeCommutativeGroup group, ExtensionalIndexExpressionsSet indexExpressions, Expression indicesCondition, Expression body, Context context) throws Error {
		context = context.extendWith(indexExpressions);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		return solve(group, indices, indicesCondition, body, context);
	}

	@Override
	public Expression solve(MultiQuantifierEliminationProblem problem, Context context) {

		AssociativeCommutativeGroup group = problem.getGroup();
		Expression summand = makeSummand(problem, context);
		Iterator<Assignment> assignmentsIterator = makeAssignmentsIterator(problem.getIndices(), problem.getConstraint(), context);

		IterativeAdder adder = 
				new IterativeAdder(
						group,
						assignmentsIterator,
						summand,
						topRewriterUsingContextAssignments,
						context);

		Expression currentValue = group.additiveIdentityElement();		
		for (Expression value : in(adder)) {
			currentValue = value;
			if (group.isAdditiveAbsorbingElement(currentValue)) {
				break;
			}
		}
		
		Expression result = normalizeIfThereIsATheoryAvailable(currentValue, context);
		return result;
		
	}

	private Expression normalizeIfThereIsATheoryAvailable(Expression currentValue, Context context) {
		Expression result = context.getTheory() == null? currentValue : context.getTheory().evaluate(currentValue, context);
		return result;
	}
}