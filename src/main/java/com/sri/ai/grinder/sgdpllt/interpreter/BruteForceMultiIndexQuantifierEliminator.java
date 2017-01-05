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

import static com.sri.ai.grinder.helper.GrinderUtil.extendRegistryWithIndexExpressions;
import static com.sri.ai.util.Util.in;

import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractMultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;

/**
 * An extension of {@link AbstractMultiIndexQuantifierEliminator}
 * that solves quantified expressions by brute force.
 * <p>
 * Additionally, it takes an assignment to symbols as a constructing parameter,
 * and throws an error when a symbol with unassigned value is found.
 *
 * @author braz
 *
 */
@Beta
public class BruteForceMultiIndexQuantifierEliminator extends AbstractMultiIndexQuantifierEliminator {

	protected TopRewriterWithAssignment topRewriterWithBaseAssignment;

	public BruteForceMultiIndexQuantifierEliminator(TopRewriter topRewriter) {
		this(new DefaultTopRewriterWithAssignment(topRewriter));
	}
	
	public BruteForceMultiIndexQuantifierEliminator(TopRewriterWithAssignment topRewriterWithBaseAssignment) {
		super();
		this.topRewriterWithBaseAssignment = topRewriterWithBaseAssignment;
	}
	
	@Override
	public Expression solve(
			AssociativeCommutativeGroup group, 
			ExtensionalIndexExpressionsSet indexExpressions, 
			Expression indicesCondition, 
			Expression body, 
			Context context) throws Error {
		
		context = (Context) extendRegistryWithIndexExpressions(indexExpressions, context);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		return solve(group, indices, indicesCondition, body, context);
	}

	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression additiveIdentityValue = group.additiveIdentityElement();
		Expression value = additiveIdentityValue;
		Expression bodyWithCondition = IfThenElse.make(indicesCondition, body, additiveIdentityValue);
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(indices, context);
		for (Map<Expression, Expression> values : in(assignmentsIterator)) {
			TopRewriterWithAssignment extended = topRewriterWithBaseAssignment.extendWith(values);
			Rewriter rewriter = new Recursive(new Exhaustive(extended));
			Expression bodyEvaluation = rewriter.apply(bodyWithCondition, context);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			value = group.add(value, bodyEvaluation, context);
		}
		return value;
	}
}