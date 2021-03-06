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
package com.sri.ai.grinder.api;

import static com.sri.ai.expresso.api.Tuple.tuple;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminationProblem.makeProblem;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Triple;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface MultiQuantifierEliminator {

	default Expression extendContextAndSolve(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			Context context) {

		Triple<Context, ExtensionalIndexExpressionsSet, Expression> extension
		= context.extendWith(indexExpressions, tuple(indicesCondition, body));
		context = extension.first;
		indexExpressions = extension.second;
		indicesCondition = extension.third.get(0);
		body             = extension.third.get(1);
		
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression quantifierFreeExpression = solve(group, indices, indicesCondition, body, context);
		return quantifierFreeExpression;
	}
	
	/**
	 * Convenience substitute for {@link #extendContextAndSolve(AssociativeCommutativeGroup, Expression, Expression, Collection, Context)}
	 * assuming a true constraint.
	 */
	default Expression extendContextAndSolve(AssociativeCommutativeGroup group, List<Expression> indices, Expression body, Context context) {
		Expression result = solve(group, indices, TRUE, body, context);
		return result;
	}

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	default Expression solve(AssociativeCommutativeGroup group, List<? extends Expression> indices, Expression indicesConstraint, Expression body, Context context) {
		MultiQuantifierEliminationProblem problem = makeProblem(group, indices, indicesConstraint, body, context);
		Expression result = solve(problem, context);
		return result;
	}
	
	/**
	 * Returns the given problem's answer.
	 */
	Expression solve(MultiQuantifierEliminationProblem problem, Context context);
	
	void interrupt();
	
	boolean getDebug();

	void setDebug(boolean newValue);
	
	// Convenience:
	
	public default Expression solveSingleIndexQuantifierEliminationProblem(SingleQuantifierEliminationProblem problem, Context context) {
		AssociativeCommutativeGroup group = problem.getGroup();
		LinkedList<Expression> indices = list(problem.getIndex());
		Expression indicesCondition = problem.getConstraint();
		Expression body = problem.getBody();
		
		Expression result = solve(group, indices, indicesCondition, body, context);
		
		return result;
	}
}