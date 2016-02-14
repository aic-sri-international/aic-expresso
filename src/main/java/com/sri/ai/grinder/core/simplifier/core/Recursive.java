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
package com.sri.ai.grinder.core.simplifier.core;

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.sameInstancesInSameIterableOrder;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.simplifier.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.api.TopSimplifier;

/**
 * A {@link Simplifier} that recursively applies a base simplifier <code>S</code> to a given expression.
 * <p>
 * Note that typically it makes sense that <code>S</code> is a {@link TopSimplifier},
 * that is, a simplifier that does not recurse into sub-expressions,
 * since that is precisely what {@link Recursive} is meant to do.
 * Passing a recursive simplifier to this class would probably result in the
 * same expressions but a lot less efficiently.
 * To avoid this from happening inadvertently, we statically enforce it.
 * <p>
 * TODO: CURRENTLY only applies to sub-expressions of function applications, not of, say, quantified expressions!
 * @author braz
 *
 */
@Beta
public class Recursive implements Simplifier {
	
	private Simplifier base;

	public Recursive(TopSimplifier topSimplifier) {
		super();
		this.base = topSimplifier;
	}

	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		Expression result = apply(base, expression, process);
		return result;
		// The reason we use static apply here is to enforce that the 'apply' using recursively is the static one that
		// only uses the base simplifier, as opposed to Recursive.apply, which could be overloaded and doing something else.
		// In fact, originally this is how this class was written, which created a vast difference in performance that was
		// hard to spot.
	}

	/**
	 * Applies base simplifier to top expression, then sub-expressions, and finally top expression again.
	 * 
	 * @param base
	 * @param expression
	 * @param process
	 * @return
	 */
	private static Expression apply(Simplifier base, Expression expression, RewritingProcess process) {
		Expression result = expression;
		result = base.apply(result, process);
		if (result.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			List<Expression> originalArguments = result.getArguments();
			ArrayList<Expression> simplifiedArguments =
					mapIntoArrayList(originalArguments, e -> apply(base, e, process));
			if ( ! sameInstancesInSameIterableOrder(originalArguments, simplifiedArguments)) { // this check speeds up cardinality algorithm by about 25%; it is also required for correctness wrt not returning a new instance that is equal to the input.
				result = Expressions.apply(result.getFunctor(), simplifiedArguments);
			}
			result = base.apply(result, process);
		}
	
		return result;
	}
	
}