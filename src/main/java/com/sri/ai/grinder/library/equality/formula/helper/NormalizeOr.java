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
package com.sri.ai.grinder.library.equality.formula.helper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * Performs the following normalizations on a formula:
 * 
 * or()                   -> false
 * or(..., true, ...)     -> true
 * or(..., false, ...)    -> or(..., ...)
 * or(X = Y, X = Y)       -> or(X = Y)
 * or(X = Y, ..., X != Y) -> true
 * 
 */
@Beta
public class NormalizeOr extends AbstractRewriter {
		
	public NormalizeOr() {
		this.setReifiedTests(new HasFunctor(FunctorConstants.OR));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		
		// or() -> false
		if (expression.numberOfArguments() == 0) {
			result = Expressions.FALSE;
		}
		else {
			// or(..., true, ...)  -> true
			// or(..., false, ...) -> or(..., ...)
			// or(X = Y, X = Y)    -> or(X = Y)
			Set<Expression> literalSet = new LinkedHashSet<Expression>();
			for (Expression disjunct : expression.getArguments()) {
				if (disjunct.equals(Expressions.TRUE)) {
					result = Expressions.TRUE;
					break;
				}
				else {
					if (!disjunct.equals(Expressions.FALSE)) {
						literalSet.add(disjunct);
					}
				}
			}
			if (!result.equals(Expressions.TRUE)) {
				List<Expression> literals = new ArrayList<Expression>(literalSet);
				if (literals.size() < expression.numberOfArguments()) {
					result = Or.make(literals);
				}
				else {
					// or(X = Y, ..., X != Y) -> true
					for (int i = 0; i < literals.size(); i++) {
						for (int j = i+1; j < literals.size(); j++) {
							if (FormulaUtil.isLiteral(literals.get(i), process) && 
							    FormulaUtil.isLiteral(literals.get(j), process) &&
								!literals.get(i).getFunctor().equals(literals.get(j).getFunctor()) &&
								literals.get(i).getArguments().equals(literals.get(j).getArguments())) {
								result = Expressions.TRUE;
							}
						}
					}
				}
			}
		}
		
		return result;
	}
}