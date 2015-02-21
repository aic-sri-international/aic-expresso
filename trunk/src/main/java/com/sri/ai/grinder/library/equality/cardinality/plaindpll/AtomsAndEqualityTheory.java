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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * A {@link Theory} for boolean atoms and equality literals,
 * whose splitters are either equality splitters or boolean atoms (function applications or symbols).
 * It works by using an internal {@link EqualityTheory}
 * and converting atom splitter A to either "A = true" or "A = false".
 */
public class AtomsAndEqualityTheory extends AbstractTheory {
	
	// This class deals with two types of theories and splitters;
	// first, its base equality theory and its equality splitters,
	// and itself as a theory with its own class of splitters which
	// is the union of equality splitters and boolean function applications or symbols.
	// We differentiate those two types of theory and splitters by always
	// calling the first type "equality theory" and "equality splitters".
	
	EqualityTheory equalityTheory;
	
	public AtomsAndEqualityTheory(EqualityTheory equalityTheory) {
		this.equalityTheory = equalityTheory;
	}

	@Override
	protected boolean isVariableTerm(Expression term, RewritingProcess process) {
		return equalityTheory.isVariableTerm(term, process);
	}

	@Override
	boolean splittersAlwaysHaveTwoArguments() {
		return false;
	}

	/**
	 * This implementation method is irrelevant because {@link #makeSplitterIfPossible(Expression, Collection, RewritingProcess)}
	 * is overridden and does not use it (see javadoc at this method's declaration in super class {@link AbstractTheory}).
	 */
	@Override
	protected String getCorrespondingSplitterFunctorOrNull(Expression expression) {
		throw new Error("AtomsAndEqualityTheory.getCorrespondingSplitterFunctorOrNull should never execute; see source code");
	}

	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		if (equalityTheory.isVariableTerm(expression, process)
				&&
				(
						GrinderUtil.getType(expression, process).equals(parse("Boolean"))
						||
						GrinderUtil.getType(expression, process).equals(parse("'->'(Boolean)"))
						)
				) {
			result = expression;
		}
		else {
			result = equalityTheory.makeSplitterIfPossible(expression, indices, process);
		}
		return result;
	}

	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(splitter);
		boolean result = Util.thereExists(subExpressionsIterator, e -> indices.contains(e));
		return result;
	}

	@Override
	protected BinaryFunction<Expression, RewritingProcess, Expression>
	getSplitterApplier(boolean splitterSign, Expression splitter) {
		Expression equalitySplitter     = Equality.isEquality(splitter)? splitter     : Equality.make(splitter, splitterSign);
		boolean    equalitySplitterSign = Equality.isEquality(splitter)? splitterSign : true;
		BinaryFunction<Expression, RewritingProcess, Expression> result = equalityTheory.getSplitterApplier(equalitySplitterSign, equalitySplitter);
		return result;
	}
	
	@Override
	public boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll() {
		boolean result = equalityTheory.applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll();
		return result;
	}

	@Override
	public Constraint makeConstraint(Collection<Expression> indices) {
		Constraint result = new Constraint(equalityTheory.makeConstraint(indices));
		return result;
	}

	@Override
	protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> result = equalityTheory.getFunctionApplicationSimplifiers();
		return result;
	}

	@Override
	protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> result = equalityTheory.getSyntacticFormTypeSimplifiers();
		return result;
	}
	
	private class Constraint implements Theory.Constraint {

		private AbstractEqualityTheory.Constraint equalityConstraint;
		
		public Constraint(EqualityTheory.Constraint equalityConstraint) {
			this.equalityConstraint = equalityConstraint;
		}
		
		@Override
		public Collection<Expression> getIndices() {
			return equalityConstraint.getIndices();
		}

		@Override
		public Expression pickSplitter(RewritingProcess process) {
			Expression equalitySplitter = equalityConstraint.pickSplitter(process);
			Expression result;
			if (equalitySplitter != null) {
				result = fromEqualitySplitterToSplitter(equalitySplitter);
			}
			else {
				result = null;
			}
			return result;
		}

		private Expression fromEqualitySplitterToSplitter(Expression equalitySplitter) {
			Expression result;
			if (equalitySplitter.get(1).equals(TRUE) || equalitySplitter.get(1).equals(FALSE)) {
				// equality splitters of the form "V = true" and "V = false" get translated to splitter "V".
				result = equalitySplitter.get(0);
			}
			else {
				result = equalitySplitter;
			}
			return result;
		}

		@Override
		public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
			Expression equalitySplitter = Equality.isEquality(splitter)? splitter : Equality.make(splitter, TRUE);
			Expression impliedByEqualityConstraint = equalityConstraint.normalizeSplitterGivenConstraint(equalitySplitter, process);
			Expression result;
			if (impliedByEqualityConstraint.equals(TRUE) || impliedByEqualityConstraint.equals(FALSE)) {
				result = impliedByEqualityConstraint;
			}
			else {
				result = splitter;
			}
			return result;
		}

		@Override
		public Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
			Expression equalitySplitter     = Equality.isEquality(splitter)? splitter     : Equality.make(splitter, splitterSign);
			boolean    equalitySplitterSign = Equality.isEquality(splitter)? splitterSign : true;
			EqualityTheory.Constraint newEqualityConstraint = (EqualityTheory.Constraint)
					equalityConstraint.applySplitter(equalitySplitterSign, equalitySplitter, process);
			Constraint result;
			if (newEqualityConstraint != null) {
				result = new Constraint(newEqualityConstraint);
			}
			else {
				result = null;
			}
			return result;
		}

		@Override
		public Expression modelCount(RewritingProcess process) {
			Expression equalityModelCount = equalityConstraint.modelCount(process);
			Expression result =
					equalityModelCount.replaceAllOccurrences(
							e -> fromEqualitySplitterToSplitterIfSplitterInTheFirstPlace(e, process),
							process);
			return result;
		}

		private Expression fromEqualitySplitterToSplitterIfSplitterInTheFirstPlace(Expression expression, RewritingProcess process) {
			Expression splitter = makeSplitterIfPossible(expression, equalityConstraint.getIndices(), process);
			Expression result;
			if (splitter == null) {
				result = expression;
			}
			else {
				result = fromEqualitySplitterToSplitter(splitter);
			}
			return result;
		}

		@Override
		public Expression normalize(Expression expression, RewritingProcess process) {
			Expression result = equalityConstraint.normalize(expression, process);
			return result;
		}
		
		@Override
		public String toString() {
			return "AtomsAndEqualityTheory on " + equalityConstraint;
		}
	}
}