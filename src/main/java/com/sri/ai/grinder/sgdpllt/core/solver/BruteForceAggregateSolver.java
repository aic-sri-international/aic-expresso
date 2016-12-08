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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.grinder.helper.GrinderUtil.extendRegistryWithIndexExpressions;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.sgdpllt.api.AggregateSolver;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.DefaultTopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;
import com.sri.ai.grinder.sgdpllt.simplifier.api.Simplifier;
import com.sri.ai.util.collect.StackedHashMap;
import com.sri.ai.util.collect.StackedMap;

/**
 * An extension of {@link AggregateSolver}
 * that solves quantified and aggregate expressions by brute force.
 * <p>
 * Additionally, it takes an assignment to symbols as a constructing parameter,
 * and throws an error when a symbol with unassigned value is found.
 *
 * @author braz
 *
 */
@Beta
public class BruteForceAggregateSolver implements AggregateSolver {

	/**
	 * {@link BruteForceAggregateSolver} needs a rewriter to evaluate bodies of expressions.
	 * It needs this rewriter to consider the current assignment, which is modified by {@link BruteForceAggregateSolver}
	 * at every iteration.
	 * This class serves as a super class for rewriters, ensuring that they can take
	 * an assignment from {@link BruteForceAggregateSolver} into account,
	 * It also takes a base top rewriter that includes any other desired operations.
	 * 
	 * @author braz
	 *
	 */
	public static class TopRewriterWithAssignment implements TopRewriter {

		/**
		 * We use a field to store the actual top rewriter to be used because
		 * when we update the base top rewriter, we get a whole new instance to use
		 * as top rewriter, and "this" is always the same fixed instance.
		 */
		private TopRewriter actualTopRewriter;
		
		/** The assignment to use to replace values for symbols. */
		private Map<Expression, Expression> assignment;
		
		private Switch<Object> valueReplacer = new Switch<Object>(
				Switch.SYNTACTIC_FORM_TYPE,
				map(
						Symbol.SYNTACTIC_FORM_TYPE,
						(Simplifier) (e, c) -> {
							Expression result = assignment.get(e);
							if (result == null) {
								result = e;
							}
							return result;
						}));

		/** Updates the base top rewriters used. */
		public void setBaseTopRewriter(TopRewriter baseTopRewriter) {
			this.actualTopRewriter = new DefaultTopRewriter(valueReplacer, baseTopRewriter);
		}
		
		public void setAssignment(Map<Expression, Expression> assignment) {
			this.assignment = assignment;
		}
		
		@Override
		public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
			return actualTopRewriter.makeStepSolver(expression);
		}
	}
	
	private StackedMap<Expression, Expression> totalAssignment;
	
	private Rewriter rewriter;
	
	public BruteForceAggregateSolver(TopRewriterWithAssignment topRewriterWithAssignment) {
		this(topRewriterWithAssignment, map());
	}
	
	public BruteForceAggregateSolver(TopRewriterWithAssignment topRewriterWithAssignment, Map<Expression, Expression> baseAssignment) {
		super();
		this.totalAssignment = new StackedHashMap<>(map(), baseAssignment);
		topRewriterWithAssignment.setAssignment(totalAssignment);
		this.rewriter = new Recursive(new Exhaustive(topRewriterWithAssignment));
	}
	
	@Override
	public Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group, 
			ExtensionalIndexExpressionsSet indexExpressions, 
			Expression indicesCondition, 
			Expression body, 
			Context context) throws Error {
		
		context = (Context) extendRegistryWithIndexExpressions(indexExpressions, context);
		Expression additiveIdentityValue = group.additiveIdentityElement();
		Expression value = additiveIdentityValue;
		Expression bodyWithCondition = IfThenElse.make(indicesCondition, body, additiveIdentityValue);
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(indexExpressions, context);
		for (Map<Expression, Expression> values : in(assignmentsIterator)) {
			totalAssignment.setTop(values);
			Expression bodyEvaluation = rewriter.rewrite(bodyWithCondition, context);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			value = group.add(value, bodyEvaluation, context);
		}
		return value;
	}
}