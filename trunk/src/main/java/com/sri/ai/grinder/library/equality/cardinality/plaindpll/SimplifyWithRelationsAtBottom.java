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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;

/**
 * A simplification for equalities and atoms that leaves atoms at the bottom of expressions,
 * and a selected target atom last at the bottom.
 * 
 * This is useful for converting arbitrary expressions to a format useful for
 * the 2014 PRAiSE "high-level syntax" rules.
 * We expect this format to be phased out eventually.
 * 
 * @author braz
 *
 */
@Beta
public class SimplifyWithRelationsAtBottom {

	/*
	 * This class is a little tricky, so here's the explanation.
	 * We want to perform a DPLL on equality on symbols theory splitters only first,
	 * so that equalities are on top.
	 * For the inner "unconditional" expressions (that is, without equalities,
	 * which are the only conditions under equality theory, but it may contain atoms),
	 * we want to perform a second, inner DPLL on all atoms but the ones with the target predicate
	 * (so that the target predicate is always in the lowest portions of the final expression).
	 * Finally, a third DPLL simplification is performed on the expression containing the target predicate only.
	 * 
	 * This is achieved in the following way.
	 * First, we extend DPLLGeneralizedAndSymbolic for the first DPLL defined above
	 * by fixing the theory to an equality theory on terms (and not the one that takes atoms as well),
	 * and also overriding the normalizeUnconditionalExpression method so that it invokes the
	 * second DPLL on the "unconditional" equality-free expressions.
	 * 
	 * We implement the second DPLL by extending AtomsAndEqualityOnTermsTheory's
	 * with an overridden makeSplitterIfPossible that does not consider atoms with the target
	 * predicate as splitters; this way, all other atoms will be placed above it. 
	 * 
	 * The third DPLL is on expressions with the target predicate only and can be
	 * the ordinary DPLL on atoms and equalities. 
	 */
	
	public static Expression simplify(Expression expression, Expression targetPredicate, RewritingProcess process) {
		DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget equalitiesSimplifier = new DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget(targetPredicate);
		Expression result = equalitiesSimplifier.solve(expression, Util.list() /* no indices -- simplification only */, process);
		return result;
	}

	private static class DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget extends DPLLGeneralizedAndSymbolic {

		private Expression targetPredicate;
		
		public DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget(Expression targetPredicate) {
			super(new EqualityOnTermsTheory(new SymbolTermTheory()), new Sum());
			this.targetPredicate = targetPredicate;
		}
		
		@Override
		public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
			DPLLForAtomsButTarget secondDPLL = new DPLLForAtomsButTarget(targetPredicate);
			Expression result = secondDPLL.solve(expression, Util.list(), process);
			return result;
		}
	}
	
	private static class DPLLForAtomsButTarget extends DPLLGeneralizedAndSymbolic {
		public DPLLForAtomsButTarget(Expression targetPredicate) {
			super(new AtomsButTargetAndEqualityOnTermsButTargetTheory(targetPredicate), new Sum());
		}
		
		@Override
		public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
			DPLLGeneralizedAndSymbolic thirdDPLL =
					new DPLLGeneralizedAndSymbolic(
							new AtomsAndEqualityOnTermsTheory(new EqualityOnTermsTheory(new FunctionalTermTheory())),
							new Sum());
			Expression result = thirdDPLL.solve(expression, Util.list(), process);
			return result;
		}
	}
	
	private static class AtomsButTargetAndEqualityOnTermsButTargetTheory extends AtomsAndEqualityOnTermsTheory {

		private Expression targetPredicate;
		
		public AtomsButTargetAndEqualityOnTermsButTargetTheory(Expression targetPredicate) {
			super(new EqualityOnTermsTheory(new FunctionalTermsButTargetTermTheory(targetPredicate)));
			this.targetPredicate = targetPredicate;
		}

		public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
			Expression result;;
			if (expression.hasFunctor(targetPredicate)) {
				result = null;
			}
			else {
				result = super.makeSplitterIfPossible(expression, indices, process);
			}
			return result;
		}
	}
	
	private static class FunctionalTermsButTargetTermTheory extends FunctionalTermTheory {
		
		private Expression targetPredicate;
		
		public FunctionalTermsButTargetTermTheory(Expression targetPredicate) {
			super();
			this.targetPredicate = targetPredicate;
		}

		public boolean isTerm(Expression expression, RewritingProcess process) {
			boolean result;
			if (expression.hasFunctor(targetPredicate)) {
				result = false;
			}
			else {
				result = super.isTerm(expression, process);
			}
			return result;
		}
	}
}