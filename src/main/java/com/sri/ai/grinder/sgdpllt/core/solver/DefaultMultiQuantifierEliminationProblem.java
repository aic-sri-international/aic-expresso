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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.util.Util.myAssert;

import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;

@Beta
public class DefaultMultiQuantifierEliminationProblem extends AbstractMultiQuantifierEliminationProblem {
	
	final public List<Expression> indices;
	final public List<Expression> indicesTypes;

	public DefaultMultiQuantifierEliminationProblem(
			AssociativeCommutativeGroup group, 
			List<Expression> indices, 
			List<Expression> indicesTypes, 
			Expression constraint, 
			Expression body) {
		
		super(group, constraint, body);
		myAssert(indices.size() == indicesTypes.size(), () -> "DefaultMultiQuantifierEliminationProblem requires numbers of indices and types to be the same, but got " + indices + " and " + indicesTypes);
		this.indices = indices;
		this.indicesTypes = indicesTypes;
	}
	
	public DefaultMultiQuantifierEliminationProblem(
			AssociativeCommutativeGroup group, 
			List<Expression> indices, 
			Expression indicesCondition,
			Expression body, 
			Context context) {
		this(group, indices, context.getTypeExpressions(indices), indicesCondition, body);
	}
	
	public static
	DefaultMultiQuantifierEliminationProblem
	makeProblem(
			AssociativeCommutativeGroup group, 
			List<Expression> indices, 
			Expression indicesCondition,
			Expression body, 
			Context context) {
		return new DefaultMultiQuantifierEliminationProblem(group, indices, indicesCondition, body, context);
	}
	
	@Override
	public List<Expression> getIndices() {
		return Collections.unmodifiableList(indices);
	}

	@Override
	public List<Expression> getIndicesTypes() {
		return Collections.unmodifiableList(indicesTypes);
	}

	@Override
	public DefaultMultiQuantifierEliminationProblem makeWithNewIndexConstraint(Expression newConstraint) {
		return new DefaultMultiQuantifierEliminationProblem(group, indices, indicesTypes, newConstraint, body);
	}

	@Override
	public DefaultMultiQuantifierEliminationProblem makeWithNewBody(Expression newBody) {
		return new DefaultMultiQuantifierEliminationProblem(group, indices, indicesTypes, constraint, newBody);
	}

	@Override
	public SingleQuantifierEliminationProblem getFirstIndexVersion() {
		return new DefaultSingleQuantifierEliminationProblem(this);
	}
}