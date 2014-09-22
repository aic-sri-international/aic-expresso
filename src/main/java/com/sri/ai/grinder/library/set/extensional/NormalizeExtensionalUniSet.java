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
package com.sri.ai.grinder.library.set.extensional;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.util.Util;

/**
 * A rewriter that will create a normalized version of an extensional set based
 * on a single call to its rewrite() method. This is different than
 * 'ExtensionalSet' which also normalizes extensional unisets but only makes a
 * single change at a time and is intended to be used within an exhaustive
 * rewriter and also relies on annotations associated with the process (this
 * feature is buggy and is to be removed).
 * 
 * @author oreilly
 * 
 */
@Beta
public class NormalizeExtensionalUniSet extends AbstractRewriter {

	@Override
	public String getName() {
		return "R_normalize_extensional_uniset";
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		if (Sets.isExtensionalUniSet(expression) && ExtensionalSet.cardinality(expression) > 1) {

			List<Expression> elements = ExtensionalSet.getElements(expression);

			result = rewriteInternal(elements, 0, 1, process);

			if (result.equals(expression)) {
				result = expression;
			}
		}
		return result;
	}

	//
	// PRIVATE METHODS
	//
	private Expression rewriteInternal(List<Expression> elements,
			int firstIndex, int secondIndex, RewritingProcess process) {

		Expression firstMember  = elements.get(firstIndex);
		Expression secondMember = elements.get(secondIndex);

		Expression condition;
		// in principle we should leave this equality to be resolved by other
		// rewriters,
		// but this can save a lot of time when sets contain lots of constants.
		if (process.isConstant(firstMember) && process.isConstant(secondMember)) {
			condition = Expressions.makeSymbol(firstMember.equals(secondMember));
		} 
		else {
			condition = Equality.make(firstMember, secondMember);
		}

		// this does not need to be computed if condition is the constant
		// 'false'
		Expression setAssumingTheyAreEqual = null;
		if (!condition.equals(Expressions.FALSE)) {
			List<Expression> reducedElements = Util.removeNonDestructively(
					elements, secondIndex);
			int firstSameIdx  = firstIndex;
			int secondSameIdx = secondIndex;
			if (secondSameIdx >= reducedElements.size()) {
				firstSameIdx++;
				secondSameIdx = firstSameIdx + 1;
			}
			if (firstSameIdx < reducedElements.size()
					&& secondSameIdx < reducedElements.size()) {
				setAssumingTheyAreEqual = rewriteInternal(reducedElements,
						firstSameIdx, secondSameIdx, process);
			} 
			else {
				setAssumingTheyAreEqual = ExtensionalSet
						.makeUniSet(reducedElements);
			}
		}

		// this does not need to be computed if condition is the constant 'true'
		Expression setAssumingTheyAreDistinct = null;
		if (!condition.equals(Expressions.TRUE)) {
			int firstDiffIdx  = firstIndex;
			int secondDiffIdx = secondIndex;
			secondDiffIdx++;
			if (secondDiffIdx >= elements.size()) {
				firstDiffIdx++;
				secondDiffIdx = firstDiffIdx + 1;
			}
			if (firstDiffIdx < elements.size() && secondDiffIdx < elements.size()) {
				setAssumingTheyAreDistinct = rewriteInternal(elements,
						firstDiffIdx, secondDiffIdx, process);
			} 
			else {
				setAssumingTheyAreDistinct = ExtensionalSet
						.makeUniSet(elements);
			}
		}

		Expression result = IfThenElse.make(condition, setAssumingTheyAreEqual,
				setAssumingTheyAreDistinct);

		return result;
	}
}
