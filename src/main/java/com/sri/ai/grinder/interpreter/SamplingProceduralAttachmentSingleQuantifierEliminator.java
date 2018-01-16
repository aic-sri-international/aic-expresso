/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.proceduralattachment.ProceduralAttachments.registerProceduralAttachment;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.grinder.rewriter.api.TopRewriter;

public class SamplingProceduralAttachmentSingleQuantifierEliminator extends AbstractContextAssignmentMultiQuantifierEliminator {

	private static String COUNTER_KEY = "Samplers counter key";

	private Random random;
	private SamplingAdderLazyIterator sampler;
	
	public SamplingProceduralAttachmentSingleQuantifierEliminator(
			TopRewriter topRewriter,
			Random random) {
		super(topRewriter);
		this.random = random;
	}

	public SamplingProceduralAttachmentSingleQuantifierEliminator(
			TopRewriterUsingContextAssignments topRewriterUsingContextAssignments,
			Random random) {
		super(topRewriterUsingContextAssignments);
		this.random = random;
	}

	@Override
	public Expression solve(MultiQuantifierEliminationProblem problem, Context context) {
		
		sampler =
				new SamplingAdderLazyIterator(
						problem,
						getTopRewriterUsingContextAssignments(),
						random,
						context);

		List<Expression> proceduralAttachmentParameters = getParameters(problem, context);
		int arity = proceduralAttachmentParameters.size();
		
		Symbol samplerFunctor = makeAndRegisterProceduralAttachment(arity, context);
				
		Expression samplerExpression = apply(samplerFunctor, proceduralAttachmentParameters);
		
		return samplerExpression;
	}

	private Symbol makeAndRegisterProceduralAttachment(int arity, Context context) {
		Integer counter = context.updateInplaceGlobalObject(COUNTER_KEY, () -> 0, c -> c.intValue() + 1);
		Simplifier samplerRewriter = 
				(Simplifier) (e, c) -> {
					sampler.setContext(c);
					Expression result = sampler.next();
					return result;
				};
		Symbol samplerFunctor = makeSymbol("sampler" + counter);
		registerProceduralAttachment(samplerFunctor, arity, samplerRewriter, context);
		return samplerFunctor;
	}

	private List<Expression> getParameters(MultiQuantifierEliminationProblem problem, Context context) {
		Set<Expression> freeVariables = Expressions.freeVariables(problem.getConditionedBody(), context);
		freeVariables.removeAll(problem.getIndices());
		List<Expression> proceduralAttachmentParameters = new ArrayList<>(freeVariables);
		return proceduralAttachmentParameters;
	}
}