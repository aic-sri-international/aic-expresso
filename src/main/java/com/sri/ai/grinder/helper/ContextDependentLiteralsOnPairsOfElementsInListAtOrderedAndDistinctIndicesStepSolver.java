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
package com.sri.ai.grinder.helper;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.CloneableIterator;
import com.sri.ai.util.base.OrderedPairsOfIntegersIterator;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CloneableFunctionIterator;

/**
 * A step solver for the context-dependent problem of whether all literals formed from pairs
 * of elements at distinct and ordered indices in a given list are defined by the contextual constraint.
 * <p>
 * When a non-defined literal is found, the step solver returns a {@link ItDependsOn} object
 * with sub-step solvers that will continue the search next time from the next pair,
 * assuming all previous ones are already defined.
 * 
 * @author braz
 *
 */
@Beta
public class ContextDependentLiteralsOnPairsOfElementsInListAtOrderedAndDistinctIndicesStepSolver extends ContextDependentDefinedLiteralsStepSolver {

	private List<Expression> list;
	private int i;
	private int j;
	private BinaryFunction<Expression, Expression, Expression> literalMaker;
	
	/**
	 * Creates a step solver that checks whether all literals made by a literal maker on ordered pairs
	 * of elements in given literal are already defined by the contextual constraint.
	 * @param list
	 * @param literalMaker
	 */
	public ContextDependentLiteralsOnPairsOfElementsInListAtOrderedAndDistinctIndicesStepSolver(List<Expression> list, BinaryFunction<Expression, Expression, Expression> literalMaker) {
		this(list, 0, 1, literalMaker);
	}

	/**
	 * Creates a step solver that checks whether all literals made by a literal maker on ordered pairs
	 * of elements in given literal from position <code>(i, j)</code> are already defined by the contextual constraint.
	 * @param list
	 * @param literalMaker
	 */
	public ContextDependentLiteralsOnPairsOfElementsInListAtOrderedAndDistinctIndicesStepSolver(List<Expression> list, int i, int j, BinaryFunction<Expression, Expression, Expression> literalMaker) {
		super(makeIterator(list, i, j, literalMaker));
		this.list = list; // we only keep those for the sake of clone()
		this.i = i;
		this.j = j;
		this.literalMaker = literalMaker;
	}

	private static CloneableIterator<Expression> makeIterator(List<Expression> list, int i, int j, BinaryFunction<Expression, Expression, Expression> literalMaker) {

		CloneableIterator<PairOf<Integer>> indicesIterator = new OrderedPairsOfIntegersIterator(list.size(), i, j);
		Function<PairOf<Integer>, Expression> literalMakerFromIndices =
				(pair) -> literalMaker.apply(list.get(pair.first), list.get(pair.second));
		CloneableIterator<Expression> functionIterator =
				new CloneableFunctionIterator<PairOf<Integer>, Expression>(indicesIterator, literalMakerFromIndices);
		return functionIterator;
	}

	@Override
	public ContextDependentExpressionProblemStepSolver clone() {
		return new ContextDependentLiteralsOnPairsOfElementsInListAtOrderedAndDistinctIndicesStepSolver(list, i, j, literalMaker);
	}
}