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
package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.map;

import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.interpreter.AbstractIterativeMultiIndexQuantifierElimination;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.collect.EZIterator;

/**
 * An assignments iterator that samples over a single variables domain.
 * 
 * NOTE: Do not call this class if the size of the domain being sampled from
 * is smaller than the the number of samples to be iterated over.
 * 
 * @author oreilly
 *
 */
public class AssignmentsSamplingIterator extends EZIterator<Map<Expression, Expression>> {
	private Expression index;
	private int  sampleSizeN;
	private int  currentN;
	private Type typeToSampleFrom; 
	private Expression condition;
	private Rewriter conditionRewriter;
	private Random random;
	private Context context;
	
	public AssignmentsSamplingIterator(List<Expression> indices, int sampleSizeN, Expression condition, Rewriter conditionRewriter, Random random, Context context) {
		if (indices.size() != 1) {
			throw new UnsupportedOperationException("Assignment sampling iterator only supports a single index currently, received: "+indices);
		}
		this.index             = indices.get(0);
		this.sampleSizeN       = sampleSizeN;
		this.currentN          = 0;
		this.typeToSampleFrom  = getTypeToSampleFrom(index, condition, context);
		this.condition         = condition;
		this.conditionRewriter = conditionRewriter;
		this.random            = random;
		this.context           = context;
	}
	
	@Override
	protected Map<Expression, Expression> calculateNext() {
		Map<Expression, Expression> result = null;
		
		if (currentN < sampleSizeN) {
			currentN++;
			do {
				Expression assignment         = typeToSampleFrom.sampleUniquelyNamedConstant(random);
				Context contextWithAssignment = AbstractIterativeMultiIndexQuantifierElimination.extendAssignments(map(index, assignment), context);
				Expression conditionValue     = conditionRewriter.apply(condition, contextWithAssignment);
				if (conditionValue.equals(Expressions.TRUE)) {
					result = map(index, assignment);
				}				
			} while (result == null);
		}
		
		return result;
	}
	
	public static Type getTypeToSampleFrom(Expression variable, Expression condition, Context context) {
		Type result = GrinderUtil.getType(variable, context);		
		if (!result.isSampleUniquelyNamedConstantSupported()) {
// TODO - see if we can take the condition into account to come up with a refined type based on the condition that can be sampled from (see Rodrigo's email).
			throw new IllegalArgumentException("Unable to sample "+variable+" from "+result);
		}
		
		return result;
	}
}