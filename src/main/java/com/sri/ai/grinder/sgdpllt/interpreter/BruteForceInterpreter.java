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

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAllRewriter;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExistsRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.MaxRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.ProductRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.SummationRewriter;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityByBruteForce;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An extension of {@link Rewriter}
 * that solves quantified and aggregate expressions by brute force
 * (besides using a {@link TopRewriter} given at construction).
 * <p>
 * Additionally, it takes an assignment to symbols as a constructing parameter,
 * and throws an error when a symbol with unassigned value is found.
 *
 * @author braz
 *
 */
@Beta
public class BruteForceInterpreter implements Rewriter {
	
	private TopRewriter baseTopRewriter;
	private Rewriter actualRewriter;
	private Map<Expression, Expression> assignment;
	
	public BruteForceInterpreter(TopRewriter baseTopRewriter) {
		this(baseTopRewriter, map());
	}

	public BruteForceInterpreter(TopRewriter baseTopRewriter, Map<Expression, Expression> assignment) {
		this.baseTopRewriter = baseTopRewriter;
		this.actualRewriter = new Recursive(new Exhaustive(new BruteForceTopRewriterWithAssignment(baseTopRewriter, assignment)));
		this.assignment = assignment;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		return actualRewriter.makeStepSolver(expression);
	}

	public BruteForceInterpreter extendWith(Map<Expression, Expression> moreAssignments, Context context) {
		return new BruteForceInterpreter(baseTopRewriter, new StackedHashMap<>(moreAssignments, assignment));
	}

	private class BruteForceTopRewriterWithAssignment extends TopRewriterWithAssignment {

		private TopRewriter baseTopRewriter;

		public BruteForceTopRewriterWithAssignment(TopRewriter baseTopRewriter, Map<Expression, Expression> assignment) {
			super(assignment);
			this.baseTopRewriter = baseTopRewriter;
			BruteForceMultiIndexQuantifierEliminator bruteForceQuantifierEliminator = 
					new BruteForceMultiIndexQuantifierEliminator(this);
			setBaseTopRewriter(
					TopRewriter.merge(
							baseTopRewriter,

							new SummationRewriter(bruteForceQuantifierEliminator),
							new ProductRewriter(bruteForceQuantifierEliminator),
							new MaxRewriter(bruteForceQuantifierEliminator),

							new ThereExistsRewriter(bruteForceQuantifierEliminator),
							new ForAllRewriter(bruteForceQuantifierEliminator),

							new CardinalityByBruteForce(bruteForceQuantifierEliminator)
							));
		}

		@Override
		public TopRewriterWithAssignment makeCopyWith(Map<Expression, Expression> newAssignment)  {
			TopRewriterWithAssignment result = new BruteForceTopRewriterWithAssignment(baseTopRewriter, newAssignment);
			return result;
		}
	}
}