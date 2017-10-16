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
package com.sri.ai.grinder.sgdpllt.theory.function;

import static com.sri.ai.grinder.sgdpllt.library.commonrewriters.CommonSimplifiersAndSymbolicQuantifierEliminationRewritersTopRewriter.INSTANCE;
import static com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter.merge;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.MultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceMultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.lambda.LambdaBetaReductionSimplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTranslationBasedTheory;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantExpressionStepSolver;

/**
 * A {@link Theory) for functions based on brute-force enumeration.
 * 
 * @author oreilly
 *
 */
@Beta
public class BruteForceFunctionTheory extends AbstractTranslationBasedTheory {
	
	@Override
	public TopRewriter makeTopRewriter() {
		return merge(INSTANCE, new LambdaBetaReductionSimplifier());
	}

	@Override
	public boolean isSuitableFor(Type type) {
		boolean result = type instanceof FunctionType;
		return result;
	}
	
	@Override
	public 	ExpressionLiteralSplitterStepSolver getQuantifierEliminatorStepSolver(QuantifierEliminationProblem problem, Context context) {
		
		Expression variable = problem.getIndex();
		Expression type = GrinderUtil.getTypeExpressionOfExpression(variable, context);
		Expression indexExpression = IndexExpressions.makeIndexExpression(variable, type);
		ExtensionalIndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpression);
		
		MultiIndexQuantifierEliminator quantifierEliminator =
				new BruteForceMultiIndexQuantifierEliminator(context.getTheory().getTopRewriter());
		
		Expression solution = quantifierEliminator.solve(problem.getGroup(), indexExpressionsSet, problem.getConstraint(), problem.getBody(), context);
		
		return new ConstantExpressionStepSolver(solution);
	}
}
