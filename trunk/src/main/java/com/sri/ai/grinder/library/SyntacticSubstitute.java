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
package com.sri.ai.grinder.library;

import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.ReplaceByIfEqualTo;

/**
 * A class providing a static method for substituting symbols or function applications in an expression
 * by another expression (not subject itself to the same substitution, which would either not occur or lead to an infinite recursion).
 * 
 * This is a simpler, less exhaustive version of {@link SemanticSubstitute} that takes into account only syntactic information,
 * that is, it only replaces an expression by a value if the expression is represented with the same symbols
 * as the originally searched ones. Here are a few examples to make this more clear:
 * 
 * Replacing a symbol works as one would expect:
 * 
 * Replacing <code>X</code> by 99 in <code>foo</code> returns <code>foo</code>
 * Replacing <code>X</code> by 99 in <code>X</code> returns <code>99</code>
 * 
 * However, quantifications need to be taken into account:
 * replacing <code>X</code> by 99 in <code>X + if there exists X : X = 9 then X else 0</code> returns <code>99 + if there exists X : X = 9 then 99 else 0</code>
 * because the quantified X works as a distinct variable. Its scope is limited to the if's condition,
 * so the X in the then branch corresponds to the original X.
 * 
 * When replacing a function substitution, the function's corresponding "cell" has its value replaced.
 * Here are some examples:
 * 
 * Replacing <code>f(10)</code> by 99 in <code>f(9)</code> returns <code>f(9)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(Y)</code> returns <code>99</code>
 * Replacing <code>f(10)</code> by 99 in <code>f(X)</code> returns <code>f(X)</code>
 * even though, semantically speaking, <code>X</code> could be equal to <code>10</code>.
 * Semantic substitution would have returned <code>if X = 10 then 99 else f(X)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(X)</code> returns <code>if X = Y then 99 else f(X)</code>
 *
 * When replacing a function application, quantification of either the function symbol or the arguments will prevent
 * further replacement down the expression:
 * 
 * Replacing <code>f(X)</code> by 99 in <code>f(X) + (for all g : g(f(X))) + (for all f : f(X)) + (for all X : f(X))</code>
 * returns <code>99 + (for all g : g(99)) + (for all f : f(X)) + (for all X : f(X))</code>
 * 
 * @author braz
 *
 */
@Beta
public class SyntacticSubstitute {

	public static Expression replace(Expression expression, Expression replaced, Expression replacement, RewritingProcess process) {
		Expression result =
				expression.replaceAllOccurrences(
						new ReplaceByIfEqualTo<Expression>(replacement, replaced), null,
						new SubstitutePruningPredicate(replaced, replacement, process), null,
						process);
		return result;
	}

	private static class SubstitutePruningPredicate implements PruningPredicate {
		List<Expression> allSymbolsInReplacedAndReplacement;
		
		public SubstitutePruningPredicate(Expression replaced, Expression replacement, RewritingProcess process) {
			Set<Expression> freeSymbolsInReplaced    = Expressions.freeSymbols(replaced, process);
			Set<Expression> freeSymbolsInReplacement = Expressions.freeSymbols(replacement, process);
			this.allSymbolsInReplacedAndReplacement =
					Util.union(
							freeSymbolsInReplacement,
							freeSymbolsInReplaced);
		}
		@Override
		public boolean apply(Expression expression, Function<Expression, Expression> replacementFunctionFunction, RewritingProcess process) {
			List<Expression> locallyScopedSymbols = ScopedVariables.getLocallyScopedSymbols(expression, process);
			boolean result = Util.intersect(allSymbolsInReplacedAndReplacement, locallyScopedSymbols);
			return result;
		}
	}
}
