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
package com.sri.ai.grinder.library.commonrewriters;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.library.BindingTopSimplifier;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.library.lambda.LambdaBetaReductionSimplifier;
import com.sri.ai.grinder.library.number.NumericSimplifier;
import com.sri.ai.grinder.library.set.CardinalityOfSetConstantSimplifier;
import com.sri.ai.grinder.library.set.IntensionalSetConditionTopRewriter;
import com.sri.ai.grinder.library.set.IntensionalSetFalseConditionToEmptySetTopRewriter;
import com.sri.ai.grinder.library.set.invsupport.SetExpressionIsEqualToEmptySetTopRewriter;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.theory.tuple.rewriter.TupleSimplifier;

/**
 * A {@link TopRewriter} aggregating:
 * 
 * <ul>
 * <li> {@link BindingTopSimplifier}: replaces symbols by their values(<code>=, !=</code>)
 * <li> {@link BooleanSimplifier}: boolean connectives (<code>and, or, not, <=>, =></code>) and if then else
 * <li> {@link NumericSimplifier}: arithmetic (<code>+, -, *, /</code>) and inequalities (<code><, <=, >=, ></code>)
 * <li> {@link EqualitySimplifier}: equality and disequality (<code>=, !=</code>)
 * <li> {@link InequalitySimplifier}: inequality (<code><, <=, >, >=</code>)
 * <li> {@link CardinalityOfSetConstantSimplifier}: cardinalities (must be registered in context's global objects as a function application of <code>| . |</code>).
 * <li> {@link LambdaBetaReductionSimplifier}: replaces <code>(lambda X : f(X))(E)</code> by <code>f(E)</code>.
 * <li> {@link IntensionalSetConditionTopRewriter}: evaluates intensional set conditions.
 * <li> {@link IntensionalSetFalseConditionToEmptySetTopRewriter}: reduces intensional set with false condition to empty set.
 * <li> {@link SetExpressionIsEqualToEmptySetTopRewriter}: solves equalities between set expressions in a certain language and empty set.
 * </ul>
 * 
 * @author braz
 *
 */
@Beta
public class CommonSimplifier extends CombiningTopRewriter {
	
	public CommonSimplifier() {
		super(
				"Common operations",
				new BindingTopSimplifier(),
				new BooleanSimplifier(),
				new NumericSimplifier(),
				new EqualitySimplifier(),
				new InequalitySimplifier(),
				new CardinalityOfSetConstantSimplifier(),
				new LambdaBetaReductionSimplifier(),
				new TupleSimplifier(),
				
				// sets
				new IntensionalSetConditionTopRewriter(),
				new IntensionalSetFalseConditionToEmptySetTopRewriter(),
				new SetExpressionIsEqualToEmptySetTopRewriter()
				);
	}
}